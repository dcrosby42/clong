(ns clong.ecs.entity-manager
  (:require 
     [clong.utils :refer :all]
     ))

(defn gen-eid 
  "Create unique entity ID based on gensym, like 'e1432. 
  Non-functional; return value is new and unique each time."
  [] 
  (gensym 'e))

(def eid-seq (repeatedly gen-eid))

(defn manager 
  "Return a new, empty entity manager."
  [] {})

(defn next-eid
  "Retrieve the next entity id from the manager's predestined sequence, and return an updated manager."
  [{[eid & eids-rest] :new-eids :as manager}] 
              [(assoc manager :new-eids eids-rest) eid])
                  

(defn add-entity 
  "Accepts entity manager and a seq of components, creates a new entity in the manager
  based on those components.
  Returns the updated entity manager."
  [manager & opts]
  (let [eid (gen-eid)
        component-map (assoc (apply hash-map opts) :eid eid)]
    (assoc manager eid (ref component-map))))

(defn remove-entity 
  "Remove an entity and its components from the entity manager."
  [manager eid] (dissoc manager eid))

(defn remove-entities
  "Remove entities and their components from the entity manager."
  [manager eids] (reduce remove-entity manager eids))

(defn get-entity
  "Returns the ref containing the component map for an entity in the given entity manager."
  [manager eid] (get manager eid))

(defn get-entity-components 
  "Returns the component map for an entity in the given entity manager."
  [manager eid] (deref (get manager eid)))

(defn get-entity-component 
  "Return the component of the given type for an entity in the entity-manager."
  [manager eid ctype]
  (get (get-entity-components manager eid) ctype))

(defn- has-component-type? [[_ ent-ref] component-type]
  (contains? (deref ent-ref) component-type))

(defn- entries-with-component-type [manager component-type]
  (filter #(has-component-type? %1 component-type) manager))

(defn entity-ids-with-component 
  "Finds all entities having a component of the given type and returns a seq of their ids."
  [manager component-type]
  (map key (entries-with-component-type manager component-type)))

(defn entity-id-with-component 
  "Finds first entity id having a component of the given type."
  [manager component-type]
  (first (entity-ids-with-component manager component-type)))
  
(defn entities-with-component 
  "Returns entities having a component of the given type."
  [manager component-type]
  (map val (entries-with-component-type manager component-type)))

(defn entity-with-component 
  "Returns the first entity having a component of the given type."
  [manager component-type]
  (first (entities-with-component manager component-type)))


(defn search-components 
  "Finds entities with all the given component types, returns tuples with the component values.
  The first element in the tuple is the entity id."
  [manager component-types]
  (remove (fn [xs] (some nil? xs))
          (map (fn [[eid ent-ref]]
                 (cons
                   eid
                   (select-values @ent-ref component-types)))
               manager)))

(defn search-entities
  "Return any entities containing a component-type/value pair matching component-type and component-value.
  Eg.  (seatch-entities mgr :id :red-paddle)
       => ({:eid e1403, :id :red-paddle, :position [10 50], :score 5}, ...)"
  [manager component-type component-value]
  (filter (fn [ent-ref] 
            (some #(= %1 [component-type component-value]) @ent-ref)) 
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
  (alter (get manager eid) 
         (fn [{component ctype :as components}]
           (let [component1 (apply f component args)]
             (if (nil? component1)
               (dissoc components ctype)
               (assoc components ctype component1))))))
                 
(defn set-component
  "Sets the value of the ctype component for the given entity id.
  Extra args will be passed along to f when it is applied to the component.
  The component may or may not already exist for the given entity id.
  Example: 
    (set-component manager 'e101 :slow-effect [0 -200])"
  [manager eid ctype cvalue]
  (alter (get manager eid) assoc ctype cvalue))

(defn remove-component
  "Removes the ctype component for the given entity id."
  [manager eid ctype]
  (alter (get manager eid) dissoc ctype))

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
