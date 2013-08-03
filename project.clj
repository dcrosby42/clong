(defproject clong "0.1.0-SNAPSHOT"
            :description "Tinkering w Clojure and libgdx and Graph"
            :url "https://github.com/atomicobject/"
            :license {:name "Eclipse Public License"
                      :url "http://www.eclipse.org/legal/epl-v10.html"}
            :dependencies [[org.clojure/clojure "1.4.0"]
                           [com.badlogic.gdx/gdx "0.9.9-SNAPSHOT"]
                           [com.badlogic.gdx/gdx-backend-lwjgl "0.9.9-SNAPSHOT"]
                           [prismatic/plumbing "0.1.0"]]
            :repositories [["libgdx" "http://libgdx.badlogicgames.com/nightlies/maven/"]]
            :main clong.core)
