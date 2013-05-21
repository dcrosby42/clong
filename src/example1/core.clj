(ns example1.core
  (:gen-class)
  (:import (com.badlogic.gdx Gdx ApplicationListener Input Input$Keys InputAdapter Game Screen)
     (com.badlogic.gdx.graphics GL10 Mesh VertexAttribute)
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
        (show [] ((:show opts)))
        (render [dt] 
          (do
            (.glClear Gdx/gl GL10/GL_COLOR_BUFFER_BIT)
            (if (not @broken)
              (try 
                ((:render opts) dt)
                (catch Throwable th (do 
                                      (dosync (ref-set broken true))
                                      (println "ERROR!" th)(.printStackTrace th)))))))

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
;; STATE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def base-state 
  {
   :red-paddle   {:id :red-paddle, :position [20 90], :size [12 48], :color [1 0 0 1]}
   :green-paddle {:id :green-paddle, :position [440 90], :size [12 48], :color [0 1 0 1]}
   :ball {:id :ball, :position [220 120], :size [10 10], :color [1 1 1 1] :velocity [60 0]}
   })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UPDATE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def controller-mapping
  {:red-paddle   {:up   [:held Input$Keys/W]
                  :down [:held Input$Keys/S]}
   :green-paddle {:up   [:held Input$Keys/UP]
                  :down [:held Input$Keys/DOWN]}})

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

(defn ball-collide-paddle? [ball paddle]
  (let [box (to-tlbr paddle)
        pts (to-pts (to-tlbr ball))]
    (some #(contains-pt box %1) pts)))
  

(defn update-ball [ball dt paddles goals]
  (let [{[x y] :position vel :velocity} ball
        pos [(+ (* dt (vel 0)) x) (+ (* dt (vel 1)) y)]
        next-ball (assoc ball :position pos)
        collided? (some #(ball-collide-paddle? next-ball %1) paddles)
        rev-vel [(* -1 (vel 0)) (vel 1)]
        ]
        
    (if collided? 
      (let [b (assoc ball :velocity rev-vel)]
        (do
          ;(println "Collied ball" b)
          b))
      next-ball)
  ))

(defn update-state [state dt input] 
  (let [{red :red-paddle green :green-paddle ball :ball}  state
        ured (update-paddle red input)
        ugreen (update-paddle green input)
        uball (update-ball ball dt [red green] [])
        ]
    (assoc state 
           :red-paddle ured 
           :green-paddle ugreen 
           :ball uball)
  ))

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

(defn draw-level [shape-renderer state]
  (doall(map 
          (comp (partial draw-block shape-renderer) state) 
          [:red-paddle :green-paddle :ball])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SETUP
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn sandbox-screen [state stuff]
  (let [shape-renderer (ShapeRenderer.)
        input-events (ref key-input-events)
        input-processor (my-input-processor input-events)
        next-input (fn [] (assoc @input-events :held (read-held-keys held-key-watch-list)))
        ]
    (ez-screen 
      {:show (fn [] 
               (.setInputProcessor Gdx/input input-processor))
       :render (fn [dt]
                  (let [input (next-input)] ;; Get input
                    ;; Update game state
                    (dosync 
                      (alter stuff assoc 
                             :state (alter state update-state dt input)
                             :input input
                             )
                      )

                    ;; Draw
                    (draw-level shape-renderer @state)

                    ;; Clear input state
                    (dosync (ref-set input-events key-input-events)))
                  )
       }
      )))



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
(defn bb [] (dosync (alter state assoc :ball (:ball base-state))))

(defn start []
  (let [g (ez-game)
        a (LwjglApplication. g "My Application" 480 320 false)]
    (dosync (ref-set game g)
             (ref-set app a))
    true))
    
(defn rl [] (use 'example1.core :reload))

(defn set-screen [s] (.setScreen @game s))

(defn sb [] (set-screen (sandbox-screen state stuff)))

(defn rr [] (rl)(sb))
(defn rs [] (rr)(new-state))




(defn -main [& args]
  )
