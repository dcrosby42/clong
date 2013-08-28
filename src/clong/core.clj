(ns clong.core
  (:gen-class)
  (:require 
     ; [clong.ecs.entity-manager   :as em]
     [clong.ecs.component-store   :as cs]
     [clong.ecs.components.mover :as m]
     [clong.utils :refer :all]
     [clong.box :as b]
     [clong.gdx-helpers :as gh]
     [clong.input :as in]
      )
  (:import 
     (com.badlogic.gdx Gdx Input$Keys Screen)
     (com.badlogic.gdx.graphics GL10 Mesh VertexAttribute OrthographicCamera)
     (com.badlogic.gdx.graphics.g2d SpriteBatch BitmapFont)
     (com.badlogic.gdx.graphics.glutils ShapeRenderer ShapeRenderer$ShapeType)
     (com.badlogic.gdx.backends.lwjgl LwjglApplication)
     ))


(declare modes)

(defn get-mode [manager] (:mode (deref (:meta manager))))
(defn set-mode [manager mode] (alter (:meta manager) assoc :mode mode))
(defn change-to-mode [manager new-mode-id]
  (let [old-mode-id (get-mode manager)
        old-mode    (old-mode-id modes)
        out-fn      (:out old-mode)
        new-mode    (new-mode-id modes)
        in-fn       (:in new-mode)]
    (dosync
      (set-mode (in-fn (out-fn manager)) new-mode-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; INITIALIZE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def red-laser-color [1 0.7 0.7 0.5])
(def green-laser-color [0.7 1 0.7 0.5])
(def red [1 0 0 1])
(def green [0 1 0 1])
(def white [1 1 1 1])
(def yellow [1 1 0 1])


; (defn next-eid [] (gensym 'e))
(defn new-eid-sequence [] (repeatedly #(gensym 'e)))

(defn- make-components [eid & pairs]
  (map (fn [[ctype data]] (cs/component eid ctype data))
   (apply array-map pairs)))

; (defn add-components [cstore eid & pairs]
;   (doseq [component (apply make-components eid pairs)]
;     (cs/add-component cstore component)))

; (defn add-components [cstore eid & pairs]
;   (doseq [[ctype data] (apply array-map pairs)]
;     (cs/add-component cstore (cs/component eid ctype data))))

(defn yellow-ball-entity [eid]
  (make-components eid 
                  :ball {}
                  :box {:size [10 10] 
                        :color yellow 
                        :position [40 100] 
                        :velocity [30 190]}
                   :controls {}
                   :timer {:trigger :ping, :ttl 3, :recur true, :interval 3}))

(defn game-ball [box] (assoc box :position [220 120] :velocity [30 60]))

(defn ball-entity [eid]
  (make-components eid
                  :ball {}
                  :box (game-ball {:size [10 10] :color white})))

(defn field-entity [eid]
  (make-components eid
                  :field {}
                  :box {:position [0 0] :size [480 320]}))

(defn red-goal-entity [eid]
  (make-components eid
             :goal {:score-goes-to :green-paddle}
             :box {:position [-20 0] :size [20 320]}))

(defn green-goal-entity [eid]
  (make-components eid
             :goal {:score-goes-to :red-paddle}
             :box {:position [480 0] :size [20 320]}))

(defn red-paddle-entity [eid] 
  (make-components eid
                  :paddle {:id    :red-paddle
                           :score 0}
                  :box    {:color red
                           :position [20 90] 
                           :size     [12 48] 
                           :velocity [0 0]}
                  :controls    {}
                  :controller-mapping {:action-keys {:up    [:held Input$Keys/W]
                                                    :down  [:held Input$Keys/S]
                                                    :shoot [:pressed Input$Keys/E]}}
                  ))

(defn green-paddle-entity [eid] 
  (make-components eid
                  :paddle   {:id    :green-paddle
                             :score 0}
                  :box      {:color green
                             :size [12 48] 
                             :velocity [0 0] 
                             :position [440 60]}
                  :controls {}
                  :controller-mapping {:action-keys {:up    [:held Input$Keys/UP]
                                                     :down  [:held Input$Keys/DOWN]
                                                     :shoot [:pressed Input$Keys/PERIOD]}}))
(defn game-control-entity [eid]
  (make-components eid
             :game-control []
             ; :id :game-control
             :game-mode { :mode :ready }
             :controls {}
             :controller-mapping {:start [:pressed Input$Keys/ENTER]
                                  :pause [:pressed Input$Keys/SPACE]}
))

; 
; (defn game-control-entity [manager]
;   (em/add-entity manager
;              :game-control []
;              :id :game-control
;              :controls {:start false}
;              :controller-mapping {:start [:pressed Input$Keys/ENTER]
;                                   :pause [:pressed Input$Keys/SPACE]}))
;             
; 
; (defn scored-entity [manager ttl]
;   (em/add-entity manager
;              :id :scored-timer
;              :timer { :ttl ttl }))
;           
; (defn laser-entity [manager {position :position, speed :speed, color :color, owner :owner}]
;   (em/add-entity manager
;              :laser []
;              :owner owner
;              :box {:position position
;                    :size     [36 6] 
;                    :velocity [speed 0] 
;                    :color    color}
;              :timer {:ttl 1.5}))
; 
; (defn explosion-entity [manager {:keys [position color] :or {color [1 1 1 1]} :as laser}]
;   (let [particle {:position position :size [10 10] :size-change [-8 -8] :color color :velocity [-100 200] :accel [0 -600]}]
;     (em/add-entity manager
;                :explosion []
;                :timer {:ttl 0.8}
;                :particles [(assoc particle :velocity [100 200])
;                            (assoc particle :velocity [-100 200])
;                            (assoc particle :velocity [20 0])
;                            (assoc particle :velocity [-50 300])]
;                )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SYSTEMS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn box-mover-system [cstore dt input]
  (doall (cs/map-components cstore #(alter %1 m/update-mover dt) :box)))

(defn ball-cieling-system [cstore dt input]
  (doall (cs/map-components' cstore
                             (fn [[ball ball-box] [field field-box]]
                             (let [{[ball-x ball-y] :position, [ball-w ball-h] :size [dx dy] :velocity} @ball-box
                                   {[field-x field-y] :position, [field-w field-h] :size} @field-box]
                               (if (or (>= (+ ball-y ball-h) (+ field-y field-h)) (<= ball-y field-y) false)
                                 ; If box has collided with top or bottom of field, negate vertical velocity:
                                 (alter ball-box assoc :velocity [dx (* -1 dy)]))
                               ))
                             [:ball :box] [:field :box])))

(defn ball-paddle-system [cstore dt input]
  (doall (cs/map-components' cstore
                      (fn [[ball ball-box] [paddle paddle-box]]
                          (if (b/box-piercing-box? (b/to-box @ball-box) (b/to-box @paddle-box))
                            (let [[dx dy] (:velocity @ball-box)]
                              (alter ball-box assoc :velocity [(* -1 dx) dy])
                          )))
                      [:ball :box] [:paddle :box])))

(defn- set-signal 
  ([controls signame] (set-signal controls signame true))
  ([controls signame value] (alter controls assoc-in [:signals signame] value)))

(defn- get-signal [controls signame] 
  (get-in @controls [:signals signame]))

(defn- clear-signal [controls signame]
  (set-signal controls signame false))

(defn ball-ping-system [cstore dt input]
  (doall (cs/map-components cstore
                             (fn [ball controls box]
                               (if (get-signal controls :ping)
                                 (do 
                                   (println "<<PING!>>")
                                   (alter box assoc :color red)
                                   (clear-signal controls :ping))
                                 ))
                             :ball :controls :box)))

(defn resolve-controls [input action-keys]
  (vmap (fn [k [action key-code]] 
          (contains? (action input) key-code)) action-keys))

(defn controller-system [cstore dt input]
  (doall (cs/map-components cstore 
                     (fn [controls controller-mapping]
                       (alter controls assoc :signals (resolve-controls input (:action-keys @controller-mapping))))
                     :controls :controller-mapping)))

(defn calc-paddle-velocity [signals]
    (let [speed 300
          y     (- (* speed (bool-to-int (:up signals))) (* speed (bool-to-int (:down signals))))]
      [0 y]))

(defn paddle-movement-system [cstore dt input]
  (doall (cs/map-components cstore
                     (fn [paddle box controls]
                       (alter box assoc :velocity (calc-paddle-velocity  (:signals @controls))))
                     :paddle :box :controls)))

(defn timer-controls-system [cstore dt input]
  (doall (cs/map-components cstore
                            (fn [timer controls]
                              (let [ttl' (- (:ttl @timer) dt)]
                                (if (<= ttl' 0)
                                  ; time!
                                  (do
                                    (set-signal controls (:trigger @timer))
                                    ; (alter controls assoc-in [:signals (:trigger @timer)] true)
                                    (if (:recur @timer)
                                      ; reset timer:
                                      (alter timer assoc :ttl (:interval @timer))
                                      ; clear timer:
                                      (cs/remove-component cstore timer))
                                    ) ; do
                                  ; else countdown:
                                  (alter timer assoc :ttl ttl'))))
                            :timer :controls)))
;     
; 
; (defn- update-slow-effect-components [manager dt]
;   (em/update-components manager :slow-effect
;                         (fn [{ttl :ttl :as slow-effect}]
;                           (let [ttl1 (- ttl dt)]
;                             (if (<= ttl1 0)
;                               nil
;                               (assoc slow-effect :ttl ttl1))))))
; 
; (defn slow-effect-system [manager dt input]
;   (let [manager1 (update-slow-effect-components manager dt)]
;     (em/update-components2 manager1 [:box :slow-effect]
;                            (fn [{[dx dy] :velocity :as box} {speed :speed}]
;                              (assoc box :velocity [(* dx speed) (* dy speed)])))))
; 
; (defn fire-laser [manager paddle-id box]
;     (let [{[paddle-x paddle-y] :position} box
;           x         (paddle-id {:red-paddle (+ paddle-x 12) :green-paddle (- paddle-x 36)})
;           y         (+ paddle-y 24)
;           speed     (if (= :red-paddle paddle-id) 400 -400)
;           color     (if (= :red-paddle paddle-id) red-laser-color green-laser-color)]
;       (laser-entity manager 
;           {:speed speed :position [x y] :owner paddle-id :color color})))
; 
; (defn add-lasers [manager]
;   (reduce 
;     (fn [mgr [eid controls box paddle paddle-id]] 
;       (if (:shoot controls)
;         (fire-laser mgr paddle-id box)
;         mgr))
;     manager
;     (em/search-components manager [:controls :box :paddle :id])))
; 
; ; TODO: timer system?
; (defn remove-timed-out-entities [manager component-type]
;   (let [expired-results (filter (fn [[eid timer component]] (<= (:ttl timer) 0)) 
;                          (em/search-components manager [:timer component-type]))]
;     (em/remove-entities manager (map first expired-results))))
; 
; (defn expire-lasers [manager] 
;   (remove-timed-out-entities manager :laser))
; 
; (defn clear-lasers [manager]
;   (em/remove-entities manager (em/entity-ids-with-component manager :laser)))
; 
; (defn laser-strikes-paddle? [laser paddle]
;   (b/box-piercing-box? (b/to-box (:box laser)) (b/to-box (:box paddle))))
; 
; (defn laser-paddle-hits [lasers paddles]
;   (for [laser lasers, paddle paddles
;         :when (and (laser-strikes-paddle? laser paddle) 
;                    (not (= (:owner laser) (:id paddle))))]
;     [laser paddle]))
; 
; 
; 
; (defn add-explosions [manager lasers]
;   (reduce explosion-entity 
;           manager 
;           (map :box lasers)))
; 
; (defn add-slow-effect [manager ents slow-effect]
;   (reduce (fn [mgr eid] 
;             (em/set-component mgr eid :slow-effect slow-effect))
;           manager
;           (map :eid ents)))
; 
; 
; (defn collide-lasers-paddles [manager]
;   (let [lasers (em/entities-with-component manager :laser)
;         paddles (em/entities-with-component manager :paddle)
;         hits (laser-paddle-hits lasers paddles)
;         hit-lasers (map first hits)
;         hit-paddles (map second hits)
;         done-laser-eids (map :eid hit-lasers)
;         ]
;     (-> manager
;       (add-explosions hit-lasers)
;       (add-slow-effect hit-paddles {:speed 0.2 :ttl 2})
;       (em/remove-entities done-laser-eids)
;     )))
; 
; (defn paddle-weapon-system [manager dt input]
;   (-> manager 
;     add-lasers 
;     collide-lasers-paddles 
;     expire-lasers ))
; 
; 
; 
; (defn paddle-bounds-system [manager dt input]
;   (em/update-components2 manager [:box :paddle]
;                       (fn [{[x y] :position :as box} paddle] 
;                         (assoc box :position [x (clamp 0 270 y)]))))
; 
; (defn first-overlapping-goal [goals ball]
;   (first (drop-while (fn [goal] (not (b/box-piercing-box? (b/to-box (:box ball)) (b/to-box (:box goal))))) goals)))
; 
; (defn goal-system [manager dt input]
;   (let [goals (em/entities-with-component manager :goal)
;         ball  (em/entity-with-component manager :ball)]
;     (if-let [{scorer :score-goes-to} (first-overlapping-goal goals ball)]
;       (if-let [{eid :eid} (em/search-entity manager :id scorer)]
;         (-> manager 
;           (em/update-component eid :score inc)
;           (change-to-mode :scored)
;           )
;         manager)
;       manager)))
;
(defn goal-system [cstore dt input]
  (let [res (cs/map-components' cstore 
                             (fn [[ball ball-box] [goal goal-box]]
                               (if (b/box-piercing-box? (b/to-box @ball-box) (b/to-box @goal-box))
                                 (:score-goes-to @goal)
                                 nil))
                             [:ball :box] [:goal :box])
        scorer-ids (set (filter identity res))]
    (if (> (count scorer-ids) 0)
      (do
        (doall (cs/map-components cstore
                                  (fn [paddle]
                                    (if (get scorer-ids (:id @paddle))
                                      (alter paddle (fn [p] (assoc p :score (inc (:score p)))))))
                                  :paddle))
        (doall (cs/map-components cstore
                                  (fn [game-mode]
                                    (alter game-mode assoc :mode :scored))
                                  :game-mode))
        )

      )
    )
  )

; 
; (defn game-control-system [manager dt input]
;   (let [mode (get-mode manager)
;         [[_ controls] & _] (em/search-components manager [:controls :game-control])]
;     (case mode
;       :ready (if (:start controls) (change-to-mode manager :playing) manager)
;       :playing (if (:pause controls) (change-to-mode manager :paused) manager)
;       :paused (if (:pause controls) (change-to-mode manager :playing) manager)
;       :scored (if (or (:start controls) (:pause controls)) (change-to-mode manager :ready) manager)
;       manager)))
; 
; (defn timer-system [manager dt input]
;   (em/update-components manager :timer (fn [{ttl :ttl :as timer}] 
;                                          (assoc timer :ttl (- ttl dt)))))
; 
; (defn scored-system [manager dt input]
;   (let [timer (:timer (em/search-entity manager :id :scored-timer))]
;     (if (<= (:ttl timer) 0)
;       (change-to-mode manager :ready)
;       manager)))
; 
; 
; (defn expire-explosions [manager]
;   (remove-timed-out-entities manager :explosion))
; 
; (defn update-explosions [manager dt]
;   (em/update-components manager :particles
;                         (fn [particles] (map #(m/update-mover %1 dt) particles))))
; 
; (defn explosion-system [manager dt input]
;   (-> manager 
;     (expire-explosions)
;     (update-explosions dt)
;     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UPDATE MODES
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn system-chain [& systems]
  (fn [cstore dt input]
    (doseq [sys systems] (sys cstore dt input))))
    ; (reduce (fn [cstore sys] (sys cstore dt input)) cstore systems)))

(def default-transition identity)

(defn default-update [manager dt input] manager)

(def default-mode
  {:in     default-transition
   :update default-update
   :out    default-transition})

; (def froze-mode
;   (assoc default-mode :update (system-chain 
;                              controller-system
;                              game-control-system
;                              )))
; 
; (def ready-mode 
;   (assoc froze-mode
;          :in (fn [manager] 
;                (-> manager
;                  (em/update-components2 [:box :ball]
;                                         (fn [box _] (reset-ball box)))
;                  (em/update-components2 [:box :paddle]
;                                         (fn [{[x y] :position :as box} _] 
;                                           (assoc box :position [x 120])))
;                  (clear-lasers)
;                ))))
; 
; (def playing-mode
;   (assoc default-mode :update (system-chain 
;                                 controller-system
;                                 paddle-movement-system
;                                 slow-effect-system
;                                 paddle-weapon-system
;                                 box-mover-system
;                                 explosion-system
;                                 paddle-bounds-system
;                                 ball-cieling-system
;                                 ball-paddle-system
;                                 goal-system
;                                 game-control-system
;                                 timer-system
;                              )))

(def sample-mode
  (assoc default-mode :update (system-chain 
                                controller-system
                                timer-controls-system
                                paddle-movement-system
                                box-mover-system
                                ball-cieling-system
                                ball-paddle-system
                                ball-ping-system
                                goal-system
                             )))

; (def paused-mode froze-mode)
; 
; (def scored-mode 
;   (assoc default-mode 
;          :in (fn [manager]
;                (scored-entity manager 2.0))
;          :update (system-chain 
;                    timer-system
;                    scored-system)
;          :out (fn [manager]
;                 (let [st (em/search-entity manager :id :scored-timer)]
;                   (em/remove-entity manager (:eid st))))))
; 
; 
(def modes {
            ; :ready   ready-mode
;             :playing playing-mode
;             :paused  paused-mode
;             :scored  scored-mode
            :sample-mode sample-mode
            })

    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DRAWING
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn draw-block [shape-renderer {[x y] :position [w h] :size [r g b a] :color :as box}]
  (doto shape-renderer
    (.begin ShapeRenderer$ShapeType/Filled)
    (.setColor r g b a)
    (.rect x y w h)
    (.end)))

(def game-mode-strings {:playing "" :paused "PAUSED" :ready "Ready (Hit <Enter>)" :scored "** SCORE! **"})
; 
; (defn hud-rendering-system [manager dt input fw-objs]
;   (let [{camera       :camera
;          font         :font
;          sprite-batch :sprite-batch} fw-objs
;         red-score     (:score (em/search-entity manager :id :red-paddle))
;         green-score     (:score (em/search-entity manager :id :green-paddle))
;         game-mode-string (get game-mode-strings (get-mode manager))]
;     (.setProjectionMatrix sprite-batch (.combined camera))
;     (.begin sprite-batch)
; 
;     (.draw font sprite-batch (str "Red: " red-score) 20 20)
;     (.draw font sprite-batch (str "Green: " green-score) 400 20)
;     (.draw font sprite-batch game-mode-string 220 20)
; 
;     (.end sprite-batch)
;     ))
(defn hud-rendering-system [cstore dt input fw-objs]
  (let [{camera       :camera
         font         :font
         sprite-batch :sprite-batch} fw-objs
        paddle-idx    (reduce (fn [m paddle]
                                (assoc m (:id paddle) paddle))
                              {}
                              (cs/map-components cstore deref :paddle))
        game-mode     (first (cs/map-components cstore deref :game-mode))

        red-score        (get-in paddle-idx [:red-paddle :score])
        green-score      (get-in paddle-idx [:green-paddle :score])
        game-mode-string (or (get game-mode-strings (:mode game-mode)) "??")]

    (.setProjectionMatrix sprite-batch (.combined camera))
    (.begin sprite-batch)

    (.draw font sprite-batch (str "Red: " red-score) 20 20)
    (.draw font sprite-batch (str "Green: " green-score) 400 20)
    (.draw font sprite-batch game-mode-string 220 20)

    (.end sprite-batch)
    ))


(defn box-rendering-system [cstore dt input {shape-renderer :shape-renderer :as fw-objs}]
  (doall (cs/map-components cstore 
                            (fn [box] 
                              (if (:color @box) (draw-block shape-renderer @box))) 
                            :box)))

; (defn box-particle-rendering-system [manager dt input {shape-renderer :shape-renderer :as fw-objs}]
;   (let [particles (mapcat :particles (em/entities-with-component manager :particles))]
;     (doseq [box particles]
;       (draw-block shape-renderer box))))


(def side-effector-systems 
  [box-rendering-system
   hud-rendering-system])
  ; [box-rendering-system
  ;  box-particle-rendering-system
  ;  hud-rendering-system])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SETUP
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn held-keys [action-keys]
  (map (comp second second) 
       (filter (fn [[ctrl [act keycode]]] (= :held act)) 
               action-keys)))

(defn held-key-watch-list [cstore]
  (let [action-keys (cs/map-components cstore (fn [cm] (:action-keys @cm)) :controller-mapping)]
        (mapcat held-keys action-keys)))

(defn pong-screen [component-store snapshot]
  (let [fw-objs (ref {})
        input-events (ref in/key-input-events)
        input-processor (in/input-processor input-events)
        watch-list (held-key-watch-list component-store)
        next-input (fn [ie] (assoc ie :held (in/read-held-keys watch-list)))
        ]
    (gh/ez-screen 
      {:show (fn [] 
               (.setInputProcessor Gdx/input input-processor)
               (dosync
                 ;; Build "framework objects", things that are needed to tie into GDX, 
                 ;; which cannot be built until we're in a running app, on the app thread.
                 (ref-set fw-objs {:shape-renderer (ShapeRenderer.)
                                   :camera         (gh/ortho-camera 480 320)
                                   :font           (BitmapFont.)
                                   :sprite-batch   (SpriteBatch.)})))

       :render (fn [dt]
                 (dosync 
                   (let [input1 (next-input @input-events)            ;; Collect input
                         mode-fns  (:sample-mode modes)
                         update-fn (get mode-fns :update)]

                     (update-fn component-store dt input1) ;; Update game state

                     ;; Clear input:
                     (ref-set input-events in/key-input-events)  

                     ;; Update snapshot:
                     (ref-set snapshot {:input input1
                                        :component-store component-store}
                     ))

                 ;; exec all side-effector systems:
                 (doseq [system side-effector-systems] (system component-store dt (get snapshot :input) @fw-objs))))
       }
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; STATE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (def base-entity-manager (-> (em/manager)
;                            (assoc :meta (ref {:mode :ready}))
; 
;                            (ball-entity)
;                            (field-entity)
;                            (red-goal-entity)
;                            (green-goal-entity)
;                            (red-paddle-entity)
;                            (green-paddle-entity)
; 
;                            (game-control-entity)
; 
;                            (change-to-mode :ready)
;                            ))
(defn base-component-store []
  (dosync 
    (let [cstore (cs/component-store)
          constructors [
                    yellow-ball-entity
                    ball-entity
                    field-entity
                    red-paddle-entity
                    green-paddle-entity
                    red-goal-entity
                    green-goal-entity
                    game-control-entity
                    ]
          comps (mapcat (fn [f eid] (f eid) ) constructors (new-eid-sequence))
          ]

      (doseq [c comps] (cs/add-component cstore c))
      cstore)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; REPL STUFF
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce game (gh/ez-game))
(defonce app (LwjglApplication. game "Clong!" 480 320 false))

(defn set-screen! [s] (.postRunnable app (fn [] (.setScreen game s))))

; (defonce entity-manager (ref base-entity-manager))
; (defn reset-entity-manager! [] (dosync (ref-set entity-manager base-entity-manager)))
; (defonce snapshot (ref {:input nil :entity-manager nil}))

(defonce component-store-ref (ref (base-component-store)))
(defn reset-component-store! [] (dosync (ref-set component-store-ref (base-component-store))))
;;(reset-component-store!)
(defonce snapshot (ref {:input nil :component-store nil}))

(defn reset-screen! [] 
  (reset-component-store!)
  (set-screen! (pong-screen @component-store-ref snapshot)))
  ; (set-screen! (pong-screen (base-component-store) snapshot)))

; (defn rl [] (require 'clong.utils 'clong.gdx-helpers 'clong.input 'clong.box 'clong.ecs.entity-manager 'clong.ecs.components.mover :reload)(use 'clong.core :reload))
(defn rl [] 
  (require 'clong.utils 'clong.gdx-helpers 'clong.input 'clong.box 'clong.ecs.entity-manager 'clong.ecs.components.mover 'clong.ecs.component-store :reload)
  (use 'clong.core :reload))

;(defn rr [] (rl)(reset-screen!))
;(defn rs [] (rr)(reset-component-store!))

(reset-screen!)
(def cstore @component-store-ref)

(defn -main [& args]
  )

