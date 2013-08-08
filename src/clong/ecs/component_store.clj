(ns clong.ecs.component-store
  (:require 
     [clong.utils :refer :all]
     [plumbing.core :refer :all]
     ))

(defn component-store [] {})

(defn component [eid type & [attrs]]
  (ref (merge attrs {:eid eid :type type})))

(defn add-component 
  [cstore component-ref] 
  (let [component-type (get @component-ref :type)
        eid            (get @component-ref :eid)]
    (update-in cstore [component-type eid] #(cons component-ref %1))))

(defn get-components-for-entity 
  "Get the components of component-type for the given entity.
  Returns list of component refs, or empty list if entity not found or entity has no such components."
  [cstore eid component-type]
  (or (get-in cstore [component-type eid]) (list)))

(def get-component-for-entity 
  "([cstore eid component-type])
  Gets the component of component-type for the given entity.
  Returns nil if not entity not found, of if no component of the given type is attached to the entity."
  (comp first get-components-for-entity))

(defn get-components
  "Get all components of component type, independent of entity"
  [cstore component-type]
  (apply concat (vals (get cstore component-type))))

(defn map-components
  "For each entity containing components of all given component types, apply
  f to each combination of components within each entity."

  ; Single component
  ([cstore f ctype]
   (map f (get-components cstore ctype)))
  
  ; Two components
  ([cstore f ctype1 ctype2]
   (for [c1 (get-components cstore ctype1)
         c2 (get-components-for-entity cstore (:eid @c1) ctype2)]
        (f c1 c2)))

  ; Three components
  ([cstore f ctype1 ctype2 ctype3]
   (for [c1 (get-components cstore ctype1)
         c2 (get-components-for-entity cstore (:eid @c1) ctype2)
         c3 (get-components-for-entity cstore (:eid @c1) ctype3)]
     (f c1 c2 c3)))
    )

(defn update-components
  "Find and alter component refs by applying f to their current values."
  ([cstore f ctype]
     (dorun (map-components cstore #(alter %1 f) ctype))
   ))


(def tcs (let [ pairs[['e1 :box   {:name "A"}]
                      ['e1 :tiger {:name "Fred"}]
                      ['e1 :box   {:name "B"}]
                      ['e2 :truck {:name "Mater"}]
                      ['e2 :box   {:name "C"}]
                      ['e4 :box   {:name "D"}]
                      ['e5 :truck {:name "Mack"}]
                      ['e5 :box   {:name "E"}]
                      ]
               ]
           (reduce add-component 
                   (component-store) 
                   (map #(apply component %1) pairs))))
 
