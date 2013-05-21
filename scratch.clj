
(defn app-listener []
  (let [vertices (float-array [-0.5 -0.5 0 0.5 -0.5 0 0 0.5 0])
        triangles (into-array Short/TYPE [0 1 2])
        attrs (into-array VertexAttribute
                          [(VertexAttribute.
                             com.badlogic.gdx.graphics.VertexAttributes$Usage/Position
                             3 "a_position")])
        mesh (ref nil)]
    (proxy [ApplicationListener] []
      (resize [w h] )
      (show [] (println "SHOW!"))
      (create []
        (let [m (doto (Mesh. true 3 3 attrs)
                  (.setVertices vertices)
                  (.setIndices triangles))]
          (dosync ( ref-set mesh m))))
      (render []
        (doto (Gdx/gl)
          (.glClear GL10/GL_COLOR_BUFFER_BIT))
        (doto @mesh
          (.render GL10/GL_TRIANGLES 0 3)))
      (pause [] )
      (resume [] )
      (dispose [] ))))

(defn tri-screen []
  (let [vertices (float-array [-0.5 -0.5 0 0.5 -0.5 0 0 0.5 0])
        triangles (into-array Short/TYPE [0 1 2])
        attrs (into-array VertexAttribute
                          [(VertexAttribute.
                             com.badlogic.gdx.graphics.VertexAttributes$Usage/Position
                             3 "a_position")])
        mesh (ref nil)]
    (proxy [Screen] []
      (show []
        (let [m (doto (Mesh. true 3 3 attrs)
                  (.setVertices vertices)
                  (.setIndices triangles))]
          (dosync ( ref-set mesh m))))
      (render [dt]
        (doto (Gdx/gl)
          (.glClear GL10/GL_COLOR_BUFFER_BIT))
        (doto @mesh
          (.render GL10/GL_TRIANGLES 0 3)))

      (resize [w,h] (println "tri-screen.resize(" w "," h ")"))
      (hide [] (println "tri-screen.hide()"))
      (pause [] (println "tri-screen.pause()"))
      (resume [] (println "tri-screen.resume()"))
      (dispose [] (println "tri-screen.dispose()")))))
