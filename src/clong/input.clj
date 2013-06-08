(ns clong.input
  (:import 
     (com.badlogic.gdx Gdx ApplicationListener Input Input$Keys InputAdapter Game Screen)
     (com.badlogic.gdx.graphics GL10 OrthographicCamera)
     ;(com.badlogic.gdx.graphics GL10 Mesh VertexAttribute OrthographicCamera)
     ;(com.badlogic.gdx.graphics.g2d SpriteBatch BitmapFont)
     ;(com.badlogic.gdx.graphics.glutils ShapeRenderer ShapeRenderer$ShapeType)
     ;(com.badlogic.gdx.backends.lwjgl LwjglApplication)
     )
  )
(def key-input-events { :pressed #{}, :released #{}, :typed #{}, :held #{}})

(defn add-keycode-to [kies k code] (assoc kies k (conj (kies k) code)))

(defn input-processor [kies-ref]
  (proxy [InputAdapter] []
    (keyDown [keycode] (dosync (alter kies-ref add-keycode-to :pressed keycode)) true)
    (keyUp [keycode] (dosync (alter kies-ref add-keycode-to :released keycode)) true)
    (keyTyped [ch] (dosync (alter kies-ref add-keycode-to :typed ch)) true)))

(defn is-key-down? [k]
  (.isKeyPressed Gdx/input k))

(defn read-held-keys [watch-list]
  (into #{} (doall (filter is-key-down? watch-list))))

