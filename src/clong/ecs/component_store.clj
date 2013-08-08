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

(defn get-components 
  "Get the components of component-type for the given entity.
  Returns list of component refs, or empty list if entity not found or entity has no such components."
  [cstore eid component-type]
  (or (get-in cstore [component-type eid]) (list)))

(defn get-all-components
  "Get all components of component type, independent of entity"
  [cstore component-type]
  (apply concat (vals (get cstore component-type))))

(defn with-components-linked-by-entity
  "For each entity containing components of all given component types, apply
  f to each combination of components within each entity."

  ; Single component
  ([cstore f ctype]
   (map f (get-all-components cstore ctype)))
  
  ; Two components
  ([cstore f ctype1 ctype2]
   (for [c1 (get-all-components cstore ctype1)
         c2 (get-components cstore (get @c1 :eid) ctype2)]
        (f c1 c2)))

  ; Three components
  ([cstore f ctype1 ctype2 ctype3]
   (for [c1 (get-all-components cstore ctype1)
         c2 (get-components cstore (get @c1 :eid) ctype2)
         c3 (get-components cstore (get @c1 :eid) ctype3)]
     (f c1 c2 c3)))
    )

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
 
