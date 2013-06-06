(ns clong.core
  (:gen-class)
  (:import (com.badlogic.gdx Gdx ApplicationListener Input Input$Keys InputAdapter Game Screen)
     (com.badlogic.gdx.graphics GL10 Mesh VertexAttribute OrthographicCamera)
     (com.badlogic.gdx.graphics.g2d SpriteBatch BitmapFont)
     (com.badlogic.gdx.graphics.glutils ShapeRenderer ShapeRenderer$ShapeType)
     (com.badlogic.gdx.backends.lwjgl LwjglApplication)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GENERAL UTILS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn vmap [f m] 
  (apply array-map (flatten (map (fn [[k v]] [k (f k v)]) m))))

(defn clamp [lo hi v] 
  (if (< v lo) lo (if (> v hi) hi v)))

(def reject-nils (partial keep identity))

(defn btw [lo hi x] (and (>= x lo) (<= x hi)))

(defn find-by [maps k v] (first (filter #(= v (get %1 k)) maps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LIBGDX UTILS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ez-screen [opts]
  (let [opts (merge {:show    (fn [])
                     :render  (fn [dt] ) 
                     :resize  (fn [w,h] )
                     :hide    (fn [])
                     :pause   (fn [])
                     :resume  (fn [])
                     :dispose (fn [])} opts)
        
        ]
    (let [broken (ref false)]
      (proxy [Screen] []
        (show [] 
          (do
            (.glClear Gdx/gl GL10/GL_COLOR_BUFFER_BIT)
            (if (not @broken)
              (try 
                ((:show opts))
                (catch Throwable th (do 
                                      (dosync (ref-set broken true))
                                      (println "SHOW ERROR!" th)(.printStackTrace th)))))))
        (render [dt] 
          (do
            (.glClear Gdx/gl GL10/GL_COLOR_BUFFER_BIT)
            (if (not @broken)
              (try 
                ((:render opts) dt)
                (catch Throwable th (do 
                                      (dosync (ref-set broken true))
                                      (println "RENDER ERROR!" th)(.printStackTrace th)))))))

        (resize [w,h] ((:resize opts) w h))
        (hide [] ((:hide opts)))
        (pause [] ((:pause opts)))
        (resume [] ((:pause opts)))
        (dispose [] ((:pause opts)))))))

(def blank-screen (ez-screen {}))

(defn ez-game []
  (proxy [Game] []
    (create [] (.setScreen this blank-screen))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; INPUT
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def key-input-events { :pressed #{}, :released #{}, :typed #{}, :held #{}})
(defn add-keycode-to [kies k code] (assoc kies k (conj (kies k) code)))
;(defn key-code? [kies k c] (contains? (kies k) c))
;(defn key-down? [kies code] 

(defn my-input-processor [kies-ref]
  (proxy [InputAdapter] []
    (keyDown [keycode] (dosync (alter kies-ref add-keycode-to :pressed keycode)) true)
    (keyUp [keycode] (dosync (alter kies-ref add-keycode-to :released keycode)) true)
    (keyTyped [ch] (dosync (alter kies-ref add-keycode-to :typed ch)) true)))

(defn is-key-down? [k]
  (.isKeyPressed Gdx/input k))

(defn read-held-keys [watch-list]
  (into #{} (doall (filter is-key-down? watch-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; INITIALIZE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def red-laser-color [1 0.7 0.7 0.5])
(def green-laser-color [0.7 1 0.7 0.5])

(defn new-ball [] {:id :ball, :position [220 120], :size [10 10], :color [1 1 1 1] :velocity [60 30]})
(defn new-red-paddle [] {:id :red-paddle, :position [20 90], :size [12 48], :color [1 0 0 1]})
(defn new-green-paddle []  {:id :green-paddle, :position [440 90], :size [12 48], :color [0 1 0 1]})

(defn new-explosion [opts]
  (let [position (or (:position opts) [0 0])
        color    (or (:color opts) [1 1 1 1])
        base {:position position :size [10 10] :size-change [-8 -8] :color color :velocity [-100 200] :accel [0 -600]}
        opts (merge {:ttl 0.8 
                     ;:position position
                     :particles [(assoc base :velocity [100 200])
                                 (assoc base :velocity [-100 200])
                                 (assoc base :velocity [20 0])
                                 (assoc base :velocity [-50 300])]
                     } opts)]
    opts))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UPDATE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def controller-mapping
  {:red-paddle   {:up   [:held Input$Keys/W]
                  :down [:held Input$Keys/S]
                  :shoot [:pressed Input$Keys/E]}
   :green-paddle {:up   [:held Input$Keys/UP]
                  :down [:held Input$Keys/DOWN]
                  :shoot [:pressed Input$Keys/PERIOD]}
   :master       {:start [:pressed Input$Keys/ENTER]
                  :pause [:pressed Input$Keys/SPACE]
                  :test  [:pressed Input$Keys/T]
                  }
   })

(def held-key-watch-list (map #(get %1 1) (mapcat vals (vals controller-mapping))))

(defn resolve-control [input [action key-code]]
  (contains? (action input) key-code))

(defn resolve-controls [input ctrl-defs]
  (vmap (fn [k v] (resolve-control input v)) ctrl-defs))

(defn get-controls-for [input ctrl-map id]
  (resolve-controls input (get ctrl-map id)))
  
(defn bool-to-int [b] (if b 1 0))

(defn update-paddle-velocity [paddle controls]
    (let [speed (if (:slow-effect paddle) 3 10)
          y     (- (* speed (bool-to-int (:up controls))) (* speed (bool-to-int (:down controls))))]
      [0 y]))

(defn fire-paddle-laser [paddle controls]
  (if (:shoot controls)
    (let [{[paddle-x paddle-y] :position paddle-id :id} paddle
          x         (paddle-id {:red-paddle (+ paddle-x 12) :green-paddle (- paddle-x 36)})
          y         (+ paddle-y 48)
          paddle-id (:id paddle)
          speed     (if (= :red-paddle paddle-id) 20 -20)
          color     (if (= :red-paddle paddle-id) red-laser-color green-laser-color)
          laser-gesture {:speed speed :position [x y] :owner (:id paddle) :color color}]
      (assoc paddle :fire-laser laser-gesture))
    (dissoc paddle :fire-laser)))

(defn still-ttl? [x] (> (:ttl x) 0))

(defn gesture-to-laser [laser-gesture]
  (let [base (select-keys laser-gesture [:position :owner :color])
        {speed :speed} laser-gesture]
    (assoc base
           :velocity [speed 0]
           :size [36 6]
           :ttl 1.5)))

(defn add-new-lasers [paddles lasers]
  (let [gestures (reject-nils (map :fire-laser paddles))]
    (concat lasers 
            (map gesture-to-laser gestures))))

(defn update-laser [dt laser]
  (let [{[x y] :position [w h] :size [dx dy] :velocity ttl :ttl} laser
        uposition [(+ x dx)  (+ y dy)]
        uttl      (- ttl dt)
        ]
    (assoc laser 
           :position uposition 
           :ttl uttl)))

(defn update-paddle-slow-effect [paddle dt]
  (if-let [effect (:slow-effect paddle)]
    (if (< (:ttl effect) 0)
      (dissoc paddle :slow-effect)
      (update-in paddle [:slow-effect :ttl] #(- %1 dt)))
    paddle))

(defn update-paddle [paddle input ctrl-map dt]
  (let [{[x y] :position} paddle
        controls  (resolve-controls input (get ctrl-map (:id paddle)))
        pad0 (update-paddle-slow-effect paddle dt)
        [dx dy] (update-paddle-velocity pad0 controls)
        new-y (clamp 0 270 (+ y dy))
        pad1 (assoc pad0 :position [x new-y])

        pad2 (fire-paddle-laser pad1 controls)
        ]
    pad2))


(defn to-tlbr [{[x y] :position [w h] :size}]
  [(+ y h) x y (+ x w)])

(defn to-pts [[t l b r]]
  [[l t] [r t] [l b] [r b]])

(defn contains-pt? [[t l b r] [x y]]
  (and (> x l) (< x r) (> y b) (< y t)))

(defn box-piercing-box? [smaller larger]
  (let [pts (to-pts smaller)]
    (some #(contains-pt? larger %1) pts)))

(defn ball-collide-paddle? [ball paddle]
  (let [box (to-tlbr paddle)
        pts (to-pts (to-tlbr ball))]
    (some #(contains-pt? box %1) pts)))

  
(defn handle-ball-paddle-collision [ball]
  (let [{vel :velocity} ball
        vel1 [(* -1 (vel 0)) (vel 1)]]
    (assoc ball :velocity vel1)))

(defn handle-ball-top-bottom-collision [ball]
  (let [{vel :velocity} ball
        vel1 [(vel 0) (* -1 (vel 1))]
        ]
    (assoc ball :velocity vel1)))


(defn update-velocity [dt {[dx dy] :velocity [ax ay] :accel :as mover}]
  (assoc mover :velocity [(+ dx (* dt ax)) 
                          (+ dy (* dt ay))]))

;(defn update-position [dt mover]
;  (let [{[x y] :position [dx dy] :velocity} mover
;        dest [(+ (* dt dx) x) (+ (* dt dy) y)]]
;    (assoc mover :position dest)))
(defn update-position [dt {[x y] :position [dx dy] :velocity :as mover}]
  (assoc mover :position [(+ x (* dt dx))
                          (+ y (* dt dy))]))

(defn update-size [dt {[w h] :size [dw dh] :size-change :as box}]
  (assoc box :size [(+ w (* dt dw)) 
                    (+ h (* dt dh))]))

(defn collide-ball-paddles [paddles ball]
  (if (some #(ball-collide-paddle? ball %1) paddles)
    (handle-ball-paddle-collision ball)
    ball))

(defn collide-ball-top-bottom [screen-bounds ball]
  (let [{[x y] :position [w h] :size}   ball
        [s-top s-left s-bottom s-right] screen-bounds]
    (if (or (>= (+ y h) s-top) (<= y s-bottom) false)
      (handle-ball-top-bottom-collision ball)
      ball)))

(defn first-overlapping-goal [goals ball]
  (first (drop-while (fn [goal] (not (box-piercing-box? (to-tlbr ball) (to-tlbr (:body goal))))) goals)))


(defn detect-goal [goals ball]
  (if-let [goal (first-overlapping-goal goals ball)]
    (assoc ball :goal-scored-by (:scorer-for goal))
    ball))

(defn update-ball [ball dt paddles goals screen-bounds] 
  (detect-goal goals 
             (collide-ball-top-bottom screen-bounds 
                                      (collide-ball-paddles paddles
                                                            (update-position dt ball)))))
(defn score-hit [ball score]
  (if-let [player (:goal-scored-by ball)]
    (assoc score player (+ 1 (get score player)))
    score))


(defn update-mode [state input ctrl-map]
    (let [controls (resolve-controls input (:master ctrl-map))
          {mode :mode} state
          ]
      (if (:pause controls)
        (case mode
          :paused :playing 
          :playing :paused
          mode)
        (if (:start controls)
          (case mode
            :ready :playing
            :scored :ready
            mode)
          mode))))


(defn laser-strikes-paddle? [laser paddle]
  (box-piercing-box? (to-tlbr laser) (to-tlbr paddle)))

(defn laser-paddle-hits [lasers paddles]
  (for [laser lasers, paddle paddles
        :when (and (laser-strikes-paddle? laser paddle) 
                   (not (= (:owner laser) (:id paddle))))]
    [laser paddle]))

(defn apply-slow-effect-to-paddle [paddle]
  (assoc paddle :slow-effect {:ttl 1.0}))

(defn apply-slow-effect-to-paddles [hit-paddles red green]
  (let [updated (map apply-slow-effect-to-paddle hit-paddles)
        ;_ (if (not (empty? updated)) (println "slowed paddles:" updated))
        ured (or (find-by updated :id :red-paddle) red)
        ugreen (or (find-by updated :id :green-paddle) green)]
    [ured ugreen]))

(defn create-explosions [controls]
  (if (:test controls)
    [(new-explosion {:position [320 120]})]
    []))

(defn update-explosion-particle [dt particle]
  (let [
        particle1 (update-position dt (update-velocity dt (update-size dt particle)))]
    particle1))

(defn update-explosion [{ttl :ttl, particles :particles :as explosion} dt]
  (let []
    (assoc explosion 
           :particles (map (partial update-explosion-particle dt) particles)
           :ttl (- ttl dt))))


(defn update-explosions [explosions dt]
  (filter still-ttl? 
          (reject-nils 
            (map #(update-explosion %1 dt) 
                 explosions))))

(defn update-state-playing [state dt input] 
  (let [{red :red-paddle green :green-paddle ball :ball goals :goals bounds :bounds score :score lasers :lasers explosions :explosions}  state
        uball (update-ball ball dt [red green] goals bounds)
        score-event (:goal-scored-by uball)
        uscore (if score-event (score-hit uball score) score)
        umode (if score-event :scored (update-mode state input controller-mapping))
        ured (update-paddle red input controller-mapping dt)
        ugreen (update-paddle green input controller-mapping dt)

        ulasers (filter still-ttl? (map #(update-laser dt %1) lasers)) ; TODO change 'filter still-ttl' to 'remove #(< (:ttl %1) 0)' or 'remove no-ttl'
        ulasers1 (add-new-lasers [ured ugreen] ulasers)

        ; collision detection
        hits (laser-paddle-hits ulasers1 [ured ugreen])
        ; drop impacting lasers:
        ulasers2 (let [done-lasers (map first hits)]
                   ;(if (not (empty done-lasers)) (println hits))
                   (remove (fn [l] (some #{l} done-lasers)) ulasers1))
        ; slow the hit paddles:
        [ured1 ugreen1] (apply-slow-effect-to-paddles (map second hits) ured ugreen)


        ;; test explosion effect
        new-explosions (map (fn [{owner :owner :as laser}] 
                              (let [color (if (= :red-paddle owner) red-laser-color green-laser-color)]
                                (new-explosion (assoc (select-keys laser [:position]) :color color)))) 
                            (map first hits))
        ;test-explosions (create-explosions (get-controls-for input controller-mapping :master))
        ;explosions1 (concat explosions test-explosions new-explosions)
        explosions1 (concat explosions new-explosions)
        explosions2 (update-explosions explosions1 dt)
        ]
    (assoc state 
           :red-paddle ured1
           :green-paddle ugreen1
           :ball uball
           :score uscore
           :mode umode
           :lasers ulasers2
           :explosions explosions2
           )
  ))

(defn default-transition [old-state state] state)

(defn transition-to-ready [old-state state]
    (assoc state 
           :ball (new-ball)
           :red-paddle (new-red-paddle)
           :green-paddle (new-green-paddle)
           :lasers []
           ))

(def transition-to-playing default-transition)
(def transition-to-paused default-transition)
(def transition-to-scored default-transition)

(defn update-state-ready [state dt input] 
  (let [umode (update-mode state input controller-mapping)]
    (assoc state :mode umode)))

(defn update-state-paused [state dt input] 
  (let [umode (update-mode state input controller-mapping)]
    (assoc state :mode umode)
  ))

(defn update-state-scored [state dt input] 
  (let [umode (update-mode state input controller-mapping)]
    (assoc state :mode umode)
  ))


(defn update-state [state dt input] 
  (let [next-state (case (:mode state)
                     :ready (update-state-ready state dt input)
                     :playing (update-state-playing state dt input)
                     :paused (update-state-paused state dt input)
                     :scored (update-state-scored state dt input)
                     state)]
    (if (= (:mode state) (:mode next-state))
      next-state
      (case (:mode next-state)
        :ready (transition-to-ready state next-state)
        :playing (transition-to-playing state next-state)
        :paused (transition-to-paused state next-state)
        :scored (transition-to-scored state next-state)
        next-state))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DRAWING
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn draw-block [srend {[x y] :position [w h] :size [r g b a] :color}]
  (doto srend
    (.begin ShapeRenderer$ShapeType/Filled)
    (.setColor r g b a)
    (.rect x y w h)
    (.end)))

(def game-mode-strings {:playing "" :paused "PAUSED" :ready "Ready (Hit <Enter>)" :scored "** SCORE! **"})

(defn draw-hud [shape-renderer camera font sprite-batch state]
  (let [{red-score :red green-score :green} (:score state)
        {game-mode :mode} state
        ]
    (.setProjectionMatrix sprite-batch (.combined camera))
    (.begin sprite-batch)

    (.draw font sprite-batch (str "Red: " red-score) 20 20)
    (.draw font sprite-batch (str "Green: " green-score) 400 20)
    (.draw font sprite-batch ((:mode state) game-mode-strings) 220 20)

    (.end sprite-batch)
    ))


(defn draw-level [shape-renderer camera font sprite-batch state]

  ;; Draw block shapes:
  (let [stuff (map (partial get state) [:red-paddle :green-paddle :ball])
        projectiles (get state :lasers)
        explosions (mapcat :particles (get state :explosions))
        blocks (concat stuff projectiles explosions)]
    (doall
      (map (partial draw-block shape-renderer) blocks)))

  ;; Scores etc
  (draw-hud shape-renderer camera font sprite-batch state)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SETUP
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ortho-cam [screen-w screen-h]
  (let [camera (OrthographicCamera.)]
    (set! (. camera viewportHeight) screen-h)
    (set! (. camera viewportWidth) screen-w)
    (.set (.position camera) (* 0.5 screen-w) (* 0.5 screen-h) 0)
    (.update camera)
    camera))


(defn sandbox-screen [state stuff]
  (let [shape-renderer (ref nil)
        camera (ref nil)
        font (ref nil)
        sprite-batch (ref nil)
        input-events (ref key-input-events)
        input-processor (my-input-processor input-events)
        next-input (fn [] (assoc @input-events :held (read-held-keys held-key-watch-list)))
        ]
    (ez-screen 
      {:show (fn [] 
               (let [_shape-renderer (ShapeRenderer.)
                     _camera         (ortho-cam 480 320)
                     _font           (BitmapFont.)
                     _sprite-batch   (SpriteBatch.)
                     ]
                 (.setInputProcessor Gdx/input input-processor)
                 (dosync
                   (ref-set shape-renderer _shape-renderer)
                   (ref-set camera _camera)
                   (ref-set font _font)
                   (ref-set sprite-batch _sprite-batch)
                   )))

       :render (fn [dt]
                  (let [input (next-input)] ;; Get input
                    ;(if-not (= input key-input-events) (println input))
                    ;; Update game state
                    (dosync 
                      (alter stuff assoc 
                             :state (alter state update-state dt input)
                             :input input
                             )
                      )

                    ;; Draw
                    (draw-level @shape-renderer @camera @font @sprite-batch @state)

                    ;; Clear input state
                    (dosync (ref-set input-events key-input-events)))
                  )
       }
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; STATE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def base-state 
  {
   :mode :ready
   :bounds [320 0 0 480] ; t l b r
   :goals [ { :id :red-goal, :scorer-for :green, :body { :position [-20 0] :size [20 320] } }
            { :id :green-goal, :scorer-for :red, :body { :position [480 0] :size [20 320] } } ]
   :score { :red 0 :green 0 }

   :red-paddle   (new-red-paddle)
   :green-paddle (new-green-paddle)
   :ball (new-ball)
   :lasers []
   :explosions []
   })


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; REPL STUFF
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce game (ref nil))
(defonce app (ref nil))

(defonce state (ref base-state))
(defonce stuff (ref {}))
(defn new-state [] (dosync (ref-set state base-state)))
;(defn new-ball [] (:ball base-state))
(defn bb [] (dosync (alter state assoc :ball (new-ball))))

(defn start []
  (let [g (ez-game)
        a (LwjglApplication. g "My Application" 480 320 false)]
    (dosync (ref-set game g)
             (ref-set app a))
    true))
    
(defn rl [] (use 'clong.core :reload))

(defn set-screen [s] (.postRunnable @app (fn [] (.setScreen @game s))))

(defn sb [] (set-screen (sandbox-screen state stuff)))

(defn rr [] (rl)(sb))
(defn rs [] (rr)(new-state))




(defn -main [& args]
  )
