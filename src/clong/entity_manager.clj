(ns clong.entity-manager
  )


(defn gen-eid 
  "Create unique entity ID based on gensym, like 'e1432. 
  Non-functional; return value is new and unique each time."
  [] 
  (gensym 'e))

;(def eid-seq (repeatedly gen-eid))

(defn manager [] {})

;(defn entity 
;  "Accepts entity manager and a thunk that returns a seq of components.
;  Returns the updated entity manager."
;  [manager f]
;  (let [eid (gen-eid)
;        components (f)
;        ent        (apply hash-map (flatten components))]
;    (assoc manager eid ent)))

(defn entity 
  "Accepts entity manager and a seq of components, creates a new entity in the manager
  based on those components.
  Returns the updated entity manager."
  [manager components]
  (let [eid (gen-eid)
        ent (apply hash-map (apply concat components))]
    (assoc manager eid ent)))

(def get-entity get)

(def remove-entity dissoc)

(defn get-entity-component 
  [manager eid ctype]
  (get-in manager [eid ctype]))


(defn entities-with-component [manager component-type]
      (map key
           (filter (fn [mapentry] (contains? (val mapentry) component-type)) 
                   manager)))

(defn update-component
  [manager eid ctype f & args]
  (apply update-in manager [eid ctype] f args))

(defn update-components [manager component-type f & args]
  (reduce (fn [mgr eid] 
            (apply update-component mgr eid component-type f args))
          manager
          (entities-with-component manager component-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defn entity-type [tname] 
;  [:entity-type tname])

(defn mover [& opts] 
  [:mover 
   (merge {:position [0 0] :velocity [0 0] :accel [0 0]} (apply hash-map opts))])

(defn update-mover-velocity [{[dx dy] :velocity [ddx ddy] :accel :as mover} dt]
  (assoc mover :velocity [(+ dx (* dt ddx))
                          (+ dy (* dt ddy))]))

(defn update-mover-position [{[x y] :position [dx dy] :velocity :as mover} dt]
  (assoc mover :position [(+ x (* dt dx))
                          (+ y (* dt dy))]))

(defn update-mover [m dt] (update-mover-position (update-mover-velocity m dt) dt))

(defn color [r g b a]
  [:color [r g b a]])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-update-movers [mgr]
  (let [move-it (fn [mover] (assoc mover :position [5 6]))
        entities (filter (fn [e] (contains? (val e) :mover)) mgr)]
    (reduce (fn [m [eid {mover :mover}]] 
              (update-component m eid :mover move-it))
            mgr
            entities)))


(defn test-update-movers2 [manager]
  (update-components manager :mover 
                     (fn [mover] (assoc mover :position [5 6]))))
  

;(em/new mgr (fn [mgr eid]))


;{
;  'e1001 { 
;          :mover { :position [0 0] :velocity [0 0] }
;          }
;}


