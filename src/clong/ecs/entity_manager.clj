(ns clong.ecs.entity-manager
  )

(defn gen-eid 
  "Create unique entity ID based on gensym, like 'e1432. 
  Non-functional; return value is new and unique each time."
  [] 
  (gensym 'e))

;(def eid-seq (repeatedly gen-eid))

(defn manager 
  "Return a new, empty entity manager."
  [] {})

(defn entity 
  "Accepts entity manager and a seq of components, creates a new entity in the manager
  based on those components.
  Returns the updated entity manager."
  [manager & opts]
  (let [eid (gen-eid)
        ent (apply hash-map opts)]
    (assoc manager eid ent)))

(defn get-entity 
  "Returns the component map for an entity in the given entity manager."
  [manager eid] (get manager eid))

(defn remove-entity 
  "Remove an entity and its component from the entity manager."
  [manager eid] (dissoc manager eid))

(defn get-entity-component 
  "Return the component of the given type for an entity in the entity-manager."
  [manager eid ctype]
  (get-in manager [eid ctype]))


(defn entities-with-component 
  "Finds all entities having a component of the given type and returns a seq of their ids."
  [manager component-type]
  (map key
       (filter (fn [mapentry] (contains? (val mapentry) component-type)) 
               manager)))

(defn update-component
  "Applies fn f to the component of the given entity id and updates the entity manager with the result.
  Extra args will be passed along to f when it is applied to the component.
  Example: 
    (update-component manager 'e101 :mover assoc :position [10 10])
  ...changes the :position value of e101's :mover component to [10 10]"
  [manager eid ctype f & args]
  (apply update-in manager [eid ctype] f args))

(defn update-components 
  "Applies f to every component of the given type occurring in the entity manager."
  [manager component-type f & args]
  (reduce (fn [mgr eid] 
            (apply update-component mgr eid component-type f args))
          manager
          (entities-with-component manager component-type)))

(defn color [r g b a]
  [:color [r g b a]])

