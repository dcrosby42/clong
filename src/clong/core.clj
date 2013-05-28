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

(defn new-ball [] {:id :ball, :position [220 120], :size [10 10], :color [1 1 1 1] :velocity [60 30]})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UPDATE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def controller-mapping
  {:red-paddle   {:up   [:held Input$Keys/W]
                  :down [:held Input$Keys/S]}
   :green-paddle {:up   [:held Input$Keys/UP]
                  :down [:held Input$Keys/DOWN]}
   :master       {:start [:pressed Input$Keys/ENTER]
                  :pause [:pressed Input$Keys/SPACE]}
   })

(def held-key-watch-list (map #(get %1 1) (mapcat vals (vals controller-mapping))))

(defn resolve-control [input [action key-code]]
  (contains? (action input) key-code))
  
(defn bool-to-int [b] (if b 1 0))

(defn update-paddle-velocity [ctrl-map paddle input]
  (if-let [ctrl-defs  ((:id paddle) ctrl-map)]
    (let [controls (vmap (fn [k v] (bool-to-int (resolve-control input v))) ctrl-defs)
          y        (- (* 10 (:up controls)) (* 10 (:down controls)))]
      [0 y])))

(defn update-paddle [paddle input]
  (let [{[x y] :position} paddle
        [dx dy] (update-paddle-velocity controller-mapping paddle input)

        new-y (clamp 0 270 (+ y dy))
        ]
    (assoc paddle :position [x new-y])))

(defn to-tlbr [{[x y] :position [w h] :size}]
  [(+ y h) x y (+ x w)])

(defn to-pts [[t l b r]]
  [[l t] [r t] [l b] [r b]])

(defn contains-pt [[t l b r] [x y]]
  (and (> x l) (< x r) (> y b) (< y t)))

(defn box-piercing-box? [smaller larger]
  (let [pts (to-pts smaller)]
    (some #(contains-pt larger %1) pts)))

(defn ball-collide-paddle? [ball paddle]
  (let [box (to-tlbr paddle)
        pts (to-pts (to-tlbr ball))]
    (some #(contains-pt box %1) pts)))

  
(defn handle-ball-paddle-collision [ball]
  (let [{vel :velocity} ball
        vel1 [(* -1 (vel 0)) (vel 1)]]
    (assoc ball :velocity vel1)))

(defn handle-ball-top-bottom-collision [ball]
  (let [{vel :velocity} ball
        vel1 [(vel 0) (* -1 (vel 1))]
        ]
    (assoc ball :velocity vel1)))


(defn move-ball [dt ball]
  (let [{[x y] :position vel :velocity} ball
        dest [(+ (* dt (vel 0)) x) (+ (* dt (vel 1)) y)]]
    (assoc ball :position dest)))

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
                                                            (move-ball dt ball)))))
(defn score-hit [ball score]
  (if-let [player (:goal-scored-by ball)]
    (assoc score player (+ 1 (get score player)))
    score))


(defn update-mode [state input]
    (let [controls (vmap (fn [k v] (resolve-control input v)) (:master controller-mapping))
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
            mode)
          mode))))


(defn update-state-playing [state dt input] 
  (let [{red :red-paddle green :green-paddle ball :ball goals :goals bounds :bounds score :score}  state
        ured (update-paddle red input)
        ugreen (update-paddle green input)
        uball (if (<= ((:position ball) 0) 485) (update-ball ball dt [red green] goals bounds) ball)
        score-event (if (:goal-scored-by uball) true false)
        uscore (if score-event (score-hit uball score) score)
        uball1 (if score-event (new-ball) uball)
        umode (update-mode state input)
        ]
    (assoc state 
           :red-paddle ured 
           :green-paddle ugreen 
           :ball uball1
           :score uscore
           :mode umode
           )
  ))

(defn update-state-ready [state dt input] 
  (let [umode (update-mode state input)]
    (assoc state :mode umode)
  ))

(defn update-state-paused [state dt input] 
  (let [umode (update-mode state input)]
    (assoc state :mode umode)
  ))

(defn update-state [state dt input] 
  (case (:mode state)
    :ready (update-state-ready state dt input)
    :playing (update-state-playing state dt input)
    :paused (update-state-paused state dt input)
    state
    )
)


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

(defn draw-hud [shape-renderer camera font sprite-batch state]
  (let [{red-score :red green-score :green} (:score state)
        {game-mode :mode} state
        ]
    (.setProjectionMatrix sprite-batch (.combined camera))
    (.begin sprite-batch)

    (.draw font sprite-batch (str "Red: " red-score) 20 20)
    (.draw font sprite-batch (str "Green: " green-score) 400 20)
    (.draw font sprite-batch (str game-mode) 220 20)

    (.end sprite-batch)
    ))


(defn draw-level [shape-renderer camera font sprite-batch state]

  ;; Draw block shapes:
  (doall(map 
          (comp (partial draw-block shape-renderer) state) 
          [:red-paddle :green-paddle :ball]))
  
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
   :red-paddle   {:id :red-paddle, :position [20 90], :size [12 48], :color [1 0 0 1]}
   :green-paddle {:id :green-paddle, :position [440 90], :size [12 48], :color [0 1 0 1]}
   :ball (new-ball)
   :bounds [320 0 0 480] ; t l b r
   :goals [ { :id :red-goal, :scorer-for :green, :body { :position [-20 0] :size [20 320] } }
            { :id :green-goal, :scorer-for :red, :body { :position [480 0] :size [20 320] } } ]
   :score { :red 0 :green 0 }
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
