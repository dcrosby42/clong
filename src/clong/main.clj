(ns clong.main
  (:gen-class)
  (:require 
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

    
(defn rl [] (require 'clong.utils 'clong.gdx-helpers 'clong.input 'clong.box 'clong.core :reload))

(defn set-screen [s] (.postRunnable @app (fn [] (.setScreen @game s))))

(defn sb [] (set-screen (sandbox-screen state stuff)))

(defn rr [] (rl)(sb))
(defn rs [] (rr)(new-state))

(defn start []
  (let [g (gh/ez-game)
        a (LwjglApplication. g "My Application" 480 320 false)]
    (dosync (ref-set game g)
             (ref-set app a))
    (sb)
    true))


(defn -main [& args]
  )
