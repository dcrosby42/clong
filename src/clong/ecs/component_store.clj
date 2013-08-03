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


