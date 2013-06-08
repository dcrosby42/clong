(ns clong.gdx-helpers
  (:import 
     (com.badlogic.gdx Gdx ApplicationListener Input Input$Keys InputAdapter Game Screen)
     (com.badlogic.gdx.graphics GL10 OrthographicCamera)
     ;(com.badlogic.gdx.graphics GL10 Mesh VertexAttribute OrthographicCamera)
     ;(com.badlogic.gdx.graphics.g2d SpriteBatch BitmapFont)
     ;(com.badlogic.gdx.graphics.glutils ShapeRenderer ShapeRenderer$ShapeType)
     ;(com.badlogic.gdx.backends.lwjgl LwjglApplication)
     )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LIBGDX HELPERS
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
;; CAMERA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ortho-camera [screen-w screen-h]
  (let [camera (OrthographicCamera.)]
    (set! (. camera viewportHeight) screen-h)
    (set! (. camera viewportWidth) screen-w)
    (.set (.position camera) (* 0.5 screen-w) (* 0.5 screen-h) 0)
    (.update camera)
    camera))
