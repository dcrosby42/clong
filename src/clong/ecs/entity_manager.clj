(ns clong.ecs.entity-manager
  (:require 
     [clong.utils :refer :all]
     ))

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
        ent (assoc (apply hash-map opts) :eid eid)]
    (assoc manager eid ent)))

(defn get-entity 
  "Returns the component map for an entity in the given entity manager."
  [manager eid] (get manager eid))

(defn remove-entity 
  "Remove an entity and its components from the entity manager."
  [manager eid] (dissoc manager eid))

(defn remove-entities
  "Remove entities and their components from the entity manager."
  [manager eids] (reduce remove-entity manager eids))

(defn get-entity-component 
  "Return the component of the given type for an entity in the entity-manager."
  [manager eid ctype]
  (get-in manager [eid ctype]))


(defn- has-component-type? [mapentry component-type]
  (contains? (val mapentry) component-type))

(defn entity-ids-with-component 
  "Finds all entities having a component of the given type and returns a seq of their ids."
  [manager component-type]
  (map key (filter #(has-component-type? %1 component-type) manager)))

(defn entity-id-with-component 
  "Finds first entity id having a component of the given type."
  [manager component-type]
  (first (entity-ids-with-component manager component-type)))
  
(defn entities-with-component 
  "Returns entities having a component of the given type."
  [manager component-type]
  (map val
       (filter (fn [mapentry] (contains? (val mapentry) component-type)) 
               manager)))

(defn entity-with-component 
  "Returns the first entity having a component of the given type."
  [manager component-type]
  (first (entities-with-component manager component-type)))


(defn search-components 
  "Finds entities with all the given component types, returns tuples with the component values.
  The first element in the tuple is the entity id."
  [manager component-types]
  (remove (fn [xs] (some nil? xs))
          (map (fn [mapentry]
                 (cons
                   (key mapentry)
                   (select-values (val mapentry) component-types)))
               manager)))

(defn search-entities
  "Return any entities containing a component-type/value pair matching component-type and component-value.
  Eg.  (seatch-entities mgr :id :red-paddle)
       => ({:eid e1403, :id :red-paddle, :position [10 50], :score 5}, ...)"
  [manager component-type component-value]
  (filter (fn [entity] 
            (some #(= %1 [component-type component-value]) entity)) 
          (vals manager)))

(defn search-entity
  "Return the first entity containing a component-type/value pair matching component-type and component-value.
  Eg.  (seatch-entity mgr :id :red-paddle)
       => {:eid e1403, :id :red-paddle, :position [10 50], :score 5}"
  [manager component-type component-value]
  (first (search-entities manager component-type component-value)))

(defn update-component
  "Updates the value of the ctype component for the given entity id by applying fn f to the component's current value.
  Extra args will be passed along to f when it is applied to the component.
  Example: 
    (update-component manager 'e101 :mover assoc :position [10 10])
  ...changes the :position value of e101's :mover component to [10 10]"
  [manager eid ctype f & args]
  (let [component (get-in manager [eid ctype])
        component1 (apply f component args)]
    (if (nil? component1)
      (assoc manager eid (dissoc (get manager eid) ctype)) ;; no dissoc-in available by default
      (assoc-in manager [eid ctype] component1))))

(defn set-component
  "Sets the value of the ctype component for the given entity id.
  Extra args will be passed along to f when it is applied to the component.
  The component may or may not already exist for the given entity id.
  Example: 
    (set-component manager 'e101 :slow-effect [0 -200])"
  [manager eid ctype cvalue]
  (assoc-in manager [eid ctype] cvalue))

(defn remove-component
  "Removes the ctype component for the given entity id."
  [manager eid ctype]
  (update-in manager [eid] #(dissoc %1 ctype)))


(defn update-components 
  "Applies f to every component of the given type occurring in the entity manager."
  [manager component-type f & args]
  (reduce (fn [mgr eid] 
            (apply update-component mgr eid component-type f args))
          manager
          (entity-ids-with-component manager component-type)))

(defn update-components2
  "For entities containing all the given component types,
  update the first component by applying f to the found components, including any extra args."
  [manager component-types f & args]
  (reduce (fn [mgr [eid & components]]
              (apply update-component mgr eid (first component-types) f (concat (rest components) args)))
            manager
            (search-components manager component-types)))
