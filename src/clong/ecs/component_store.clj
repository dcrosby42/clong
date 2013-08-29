(ns clong.ecs.component-store
  (:require 
     [clong.utils :refer :all]
     [plumbing.core :refer :all]
     ))

(defn component-store 
  "Create a new, empty component store.
  Returns a new ref."
  [] 
  (ref {}))

(defn component 
  "Create a component with :eid and :type fields merged onto attrs-map. 
  Returns a new ref."
  [eid type & [attrs-map]]
  (ref (merge attrs-map {:eid eid :type type})))

(defn add-component 
  "Add a component (which is a ref) to an entity based on component's :eid.
  Entity will be created if needed.
  Modifies the cstore ref with resulting component store structure.
  Returns the resulting cstore value."
  [cstore component-ref] 
  (let [component-type (:type @component-ref)
        eid            (:eid  @component-ref)]
    (alter cstore update-in [component-type eid] #(cons component-ref %1))))

(defn remove-component
  "Remove the given component ref from the component store."
  [cstore component-ref]
  (alter cstore (fn [cstore]
                  (let [{component-type :type, eid :eid} @component-ref]
                    (update-in cstore [component-type eid] #(remove #{component-ref} %1))))))

(defn remove-entity
  "Remove all components associated with the given entity."
  [cstore eid]
  (doseq [comp-ref (mapcat #(get %1 eid) (vals @cstore))]
    (remove-component cstore comp-ref)))

(defn get-components-for-entity 
  "Get the components of component-type for the given entity.
  Returns list of component refs, or empty list if entity not found or entity has no such components."
  [cstore eid component-type]
  (or (get-in @cstore [component-type eid]) (list)))

(def get-component-for-entity 
  "([cstore eid component-type])
  Gets the component of component-type for the given entity.
  Returns nil if not entity not found, of if no component of the given type is attached to the entity."
  (comp first get-components-for-entity))

(defn get-components
  "Return a seq of components of the given type, independent of entity."
  [cstore component-type]
  (apply concat (vals (get @cstore component-type))))

(defn map-components
  "For each entity containing components of all given component types, apply
  f to each combination of components within each entity."

  ; Single component
  ([cstore f ctype]
   (map f (get-components cstore ctype)))
  
  ; Two components
  ([cstore f ctype1 ctype2]
   (for [c1 (get-components cstore ctype1)
         c2 (get-components-for-entity cstore (:eid @c1) ctype2)
         :when
         (not= c1 c2)]
        (f c1 c2)))

  ; Three components
  ([cstore f ctype1 ctype2 ctype3]
   (for [c1 (get-components cstore ctype1)
         c2 (get-components-for-entity cstore (:eid @c1) ctype2)
         c3 (get-components-for-entity cstore (:eid @c1) ctype3)
         :when 
         (and (not= c1 c2) (not= c1 c3) (not= c2 c3))]
     (f c1 c2 c3)))
    )

(defn map-components'
  "Combine component searches across multiple entities."

  ; Single component search
  ([cstore f s1]
   (map f (apply map-components cstore vector s1)))
  
  ; Combine two component searches
  ([cstore f s1 s2]
   (for [c1 (apply map-components cstore vector s1)
         c2 (apply map-components cstore vector s2)]
     (f c1 c2)))
  
  ; Combine three component searches
  ([cstore f s1 s2 s3]
   (for [c1 (apply map-components cstore vector s1)
         c2 (apply map-components cstore vector s2)
         c3 (apply map-components cstore vector s3)]
     (f c1 c2 c3))))

(defn update-components
  "Find and alter component refs by applying f to their current values.
  If multiple components types are specified, tuples of component values are provided to f as
  they match up within entities; only the component ref corresponding to the left-most component
  type will be altered."
  ([cstore f & ctypes]
     (dorun (apply map-components cstore 
                   (fn [ cmp1-ref & other-refs] (apply alter cmp1-ref f (map deref other-refs))) 
                   ctypes))
   ))


(def tcs 
  (let [mk-cstore (fn [cmps] (let [cstore (component-store)]
                               (dosync (doseq [c cmps] (add-component cstore c)))
                               cstore))
        mk-comps (fn [pairs] (map #(apply component %1) pairs))]
    (mk-cstore (mk-comps [['e1 :box   {:name "A"}]
                         ['e1 :tiger {:name "Fred"}]
                         ['e1 :box   {:name "B"}]
                         ['e2 :truck {:name "Mater"}]
                         ['e2 :box   {:name "C"}]
                         ['e2 :player {:name "Dave"}]
                         ['e4 :box   {:name "D"}]
                         ['e5 :truck {:name "Mack"}]
                         ['e5 :box   {:name "E"}]
                         ['ww :map   {:name "The Map"}]
                          ] ))))
(defn _go []
  (let [tellme (fn [[themap] [tiger] [truck box]] (println "Visiting" (:name themap) "for big kitteh" (:name tiger) ": trying truck" (:name truck) "whose box is" (:name box)))
        v1     (fn [a] [@a])
        v2     (fn [a b] [@a @b])
        ]

     (doseq [tiger     (map-components tcs v1 :tiger) 
             truck-box (map-components tcs v2 :truck :box) 
             themap    (map-components tcs v1 :map)] 
       (tellme themap tiger truck-box))))

