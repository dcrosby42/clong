(ns clong.ecs.components.mover
  )


(defn mover [& opts] 
  [:mover 
   (merge {:position [0 0] :velocity [0 0] :accel [0 0]} (apply hash-map opts))])

; { :position [0 0] :velocity [0 0] :accel [0 0] }
(defn update-mover-velocity [{[dx dy] :velocity [ddx ddy] :accel :as mover} dt]
  (assoc mover :velocity [(+ dx (* dt ddx))
                          (+ dy (* dt ddy))]))

(defn update-mover-position [{[x y] :position [dx dy] :velocity :as mover} dt]
  (assoc mover :position [(+ x (* dt dx))
                          (+ y (* dt dy))]))

(defn update-mover [m dt] (update-mover-position (update-mover-velocity m dt) dt))


