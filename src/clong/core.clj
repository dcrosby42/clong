(ns clong.core
  (:gen-class)
  (:require 
     [clong.ecs.entity-manager   :as em]
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

;(defn new-ball [] {:id :ball, :position [220 120], :size [10 10], :color white :velocity [60 30]})

(defn ball-entity [manager] 
  (em/entity manager 
             :ball []
             :box  {:position [220 120] :size [10 10] :velocity [30 60] :color white}))

(defn field-entity [manager]
  (em/entity manager
             :field []
             :box {:position [0 0] :size [480 320]}))
              
(defn red-goal-entity [manager]
  (em/entity manager
             :goal :green
             :box {:position [-20 0] :size [20 320]}))

(defn green-goal-entity [manager]
  (em/entity manager
             :goal :red
             :box {:position [480 0] :size [20 320]}))
              
(defn red-paddle-entity [manager] 
  (em/entity manager 
             :paddle []
             :box  {:position [20 90] :size [12 48] :velocity [0 0] :color red}
             :controls {:up false :down false :shoot false}
             :controller-mapping {:up    [:held Input$Keys/W]
                                  :down  [:held Input$Keys/S]
                                  :shoot [:pressed Input$Keys/E]}))

(defn green-paddle-entity [manager] 
  (em/entity manager 
             :paddle []
             :box  {:position [440 60] :size [12 48] :velocity [0 0] :color green}
             :controls {:up false :down false :shoot false}
             :controller-mapping {:up    [:held Input$Keys/UP]
                                  :down  [:held Input$Keys/DOWN]
                                  :shoot [:pressed Input$Keys/PERIOD]}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; STATE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(def base-state
;  { :entity-manager (em/manager) 
;   })
(def base-entity-manager (-> (em/manager)
                           (ball-entity)
                           (field-entity)
                           (red-goal-entity)
                           (green-goal-entity)
                           (red-paddle-entity)
                           (green-paddle-entity)
                           ))

(defn held-keys [controller-mapping]
  (map (comp second second) (filter (fn [[ctrl [act keycode]]] (= :held act)) controller-mapping)))

(def held-key-watch-list 
  (let [cmaps (map :controller-mapping (em/entities-with-component base-entity-manager :controller-mapping))]
    (mapcat held-keys cmaps)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SYSTEMS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn box-mover-system [manager dt input]
  (em/update-components manager :box m/update-mover dt))

(defn ball-cieling-system [manager dt input]
  (let [ball-eid (em/entity-id-with-component manager :ball)
        box      (em/get-entity-component manager ball-eid :box)
        bounds   (:box (em/entity-with-component manager :field))
        {[x y] :position [w h] :size} box
        {[left bottom] :position [bw bh] :size}  bounds]
    (if (or (>= (+ y h) (+ bottom bh)) (<= y bottom) false)
      ; If box has collided with top or bottom of field, negate vertical velocity:
      (let [{[dx dy] :velocity} box
            v1                [dx (* -1 dy)]]
        ; update the box component for the ball entity:
        (em/update-component manager ball-eid :box assoc :velocity v1))
      ; else no change:
      manager)))
    
(defn ball-paddle-system [manager dt input]
  (let [ball-eid (em/entity-id-with-component manager :ball)
        ball-box (em/get-entity-component manager ball-eid :box)
        ball-b-box (b/to-box ball-box)
        paddle-boxes (map (fn [p] (b/to-box (:box p))) (em/entities-with-component manager :paddle))]
    (if (some #(b/box-piercing-box? ball-b-box %1) paddle-boxes)
      ; ball has struck a paddle
      (let [{[dx dy] :velocity} ball-box
            v1 [(* -1 dx) dy]]
        ; negate the horiz velocity:
        (em/update-component manager ball-eid :box assoc :velocity v1))
      ; else no change:
      manager)))

(defn resolve-control [input [action key-code]]
  (contains? (action input) key-code))

(defn resolve-controls [input ctrl-defs]
  (vmap (fn [k v] (resolve-control input v)) ctrl-defs))

(defn controller-system [manager dt input]
  (let [eids (em/entity-ids-with-component manager :controller-mapping)]
    (reduce (fn [mgr eid] 
              (let [controller-mapping (em/get-entity-component mgr eid :controller-mapping)
                    controls (resolve-controls input controller-mapping)
                    ;_ (if (some true? (vals controls)) (println controls))
                    ;_ (pprint controls)
                    ]
                (em/update-component mgr eid :controls (fn [_] controls))))
            manager
            eids)))

(defn calc-paddle-velocity [paddle controls]
    (let [speed (if (:slow-effect paddle) 100 300)
          y     (- (* speed (bool-to-int (:up controls))) (* speed (bool-to-int (:down controls))))]
      [0 y]))

(defn paddle-control-system [manager dt input]
  (let [eids (em/entity-ids-with-component manager :paddle)]
    (reduce (fn [mgr eid] 
              (let [paddle (em/get-entity mgr eid)
                    controls (get paddle :controls)
                    v1 (calc-paddle-velocity paddle controls)
                    ;_ (if (some true? (vals controls)) (println "pcs:" controls "v1:" v1))
                    ]
                (em/update-component mgr eid :box assoc :velocity v1)))
            manager
            eids)))
      
;; Compose all systems:
(def systems [
              controller-system
              paddle-control-system
              box-mover-system
              ball-cieling-system
              ball-paddle-system
              ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UPDATE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn update-entity-manager [manager dt input]
  (reduce (fn [mgr sys] (sys mgr dt input)) manager systems))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DRAWING
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn draw-block [shape-renderer {[x y] :position [w h] :size [r g b a] :color}]
  ;(print "shape-renderer: ") (pprint shape-renderer)
  (doto shape-renderer
    (.begin ShapeRenderer$ShapeType/Filled)
    (.setColor r g b a)
    (.rect x y w h)
    (.end)))
;
;(def game-mode-strings {:playing "" :paused "PAUSED" :ready "Ready (Hit <Enter>)" :scored "** SCORE! **"})
;
;(defn draw-hud [shape-renderer camera font sprite-batch state]
;  (let [{red-score :red green-score :green} (:score state)
;        {game-mode :mode} state
;        ]
;    (.setProjectionMatrix sprite-batch (.combined camera))
;    (.begin sprite-batch)
;
;    (.draw font sprite-batch (str "Red: " red-score) 20 20)
;    (.draw font sprite-batch (str "Green: " green-score) 400 20)
;    (.draw font sprite-batch ((:mode state) game-mode-strings) 220 20)
;
;    (.end sprite-batch)
;    ))
;

(defn draw-level [shape-renderer camera font sprite-batch state]

  ;; Draw block shapes:
  ;(let [things (map (partial get state) [:red-paddle :green-paddle :ball])
  ;      projectiles (get state :lasers)
  ;      explosions (mapcat :particles (get state :explosions))
  ;      blocks (concat things projectiles explosions)]
  ;  (doall
  ;    (map (partial draw-block shape-renderer) blocks)))

  ;;; Scores etc
  ;(draw-hud shape-renderer camera font sprite-batch state)
  )
;
(defn rendering-system [manager dt input {shape-renderer :shape-renderer :as fw-objs}]
  (doseq [{box :box} (filter #(contains? (:box %1) :color) (map (partial em/get-entity manager) (em/entity-ids-with-component manager :box)))]
      (draw-block shape-renderer box)))

(def side-effector-systems 
  [rendering-system])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SETUP
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn pong-screen [entity-manager snapshot]
  (let [fw-objs (ref {})
        input-events (ref in/key-input-events)
        input-processor (in/input-processor input-events)
        next-input (fn [ie] (assoc ie :held (in/read-held-keys held-key-watch-list)))
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
                         entity-manager1 (alter entity-manager update-entity-manager dt input1)] ;; Update game state

                     ;; Clear input:
                     (ref-set input-events in/key-input-events)  

                     ;; Update snapshot:
                     (ref-set snapshot {:input input1
                                        :entity-manager entity-manager1})
                     ))

                 ;; exec all side-effector systems:
                 (let [em @entity-manager] 
                   (doseq [sys side-effector-systems] (sys em dt (get snapshot :input) @fw-objs))))
       }
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; REPL STUFF
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce game (gh/ez-game))
(defonce app (LwjglApplication. game "Clong!" 480 320 false))

(defn set-screen! [s] (.postRunnable app (fn [] (.setScreen game s))))

(defonce entity-manager (ref base-entity-manager))
(defn reset-entity-manager! [] (dosync (ref-set entity-manager base-entity-manager)))
(defonce snapshot (ref {:input nil :entity-manager nil}))

(defn reset-screen! [] (set-screen! (pong-screen entity-manager snapshot)))

(defn rl [] (require 'clong.utils 'clong.gdx-helpers 'clong.input 'clong.box 'clong.ecs.entity-manager 'clong.ecs.components.mover 'clong.core  :reload))
(defn rr [] (rl)(reset-screen!))
(defn rs [] (rr)(reset-entity-manager!))

(reset-screen!)


(defn -main [& args]
  )
