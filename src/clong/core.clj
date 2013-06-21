(ns clong.core
  (:gen-class)
  (:require 
     [clong.ecs.entity-manager   :as em]
     [clong.ecs.components.mover :as m]
     [clong.utils :refer :all]
     [clong.box :as box]
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
(def white [1 1 1 1])

;(defn new-ball [] {:id :ball, :position [220 120], :size [10 10], :color white :velocity [60 30]})

(defn ball-entity [manager] 
  (em/entity manager 
             [(m/mover :position [220 120] :velocity [60 30])
              ]))
              
              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; STATE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(def base-state
;  { :entity-manager (em/manager) 
;   })
(def base-entity-manager (-> (em/manager)
                           (ball-entity)))

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SYSTEMS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mover-system [manager dt input]
  (em/update-components manager :mover m/update-mover dt))


;; Compose all systems:
(def systems 
  [mover-system])

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
(defn rendering-system [manager dt input {sr :shape-renderer :as fw-objs}]
  (doseq [entity (map (partial em/get-entity manager) (em/entities-with-component manager :mover))]
    (let [{mover :mover} entity]
      (draw-block sr {:position (get mover :position) :size [10 10] :color white}))))

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
        next-input identity ;(fn [ie] (assoc ie :held (in/read-held-keys held-key-watch-list)))
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

(defn rl [] (require 'clong.utils 'clong.gdx-helpers 'clong.input 'clong.box 'clong.core :reload))
(defn rr [] (rl)(reset-screen!))
(defn rs [] (rr)(reset-entity-manager!))

(reset-screen!)


(defn -main [& args]
  )
