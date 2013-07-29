(ns clong.ecs.component-store
  (:require 
     [clong.utils :refer :all]
     ))

(defn component-store [] {})

(defn- append-component-ref [clist c]
  (cons (ref c) clist))

(defn add-component 
  [cst eid c] 
  (update-in cst [(get c :type) eid] append-component-ref (assoc c :eid eid)))

(defn get-components 
  "Get the ctype components for the given entity.
  Returns list of component refs, or empty list if entity not found or entity has no such components."
  [cst eid ctype]
  (or (get-in cst [ctype eid]) (list)))


