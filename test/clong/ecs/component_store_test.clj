(ns clong.ecs.component-store-test
  (:use clojure.test
        clojure.pprint)
  (:require [clong.ecs.component-store :as cs]))

(deftest 
  component-store-test
  (let [cstore (cs/component-store)]
    (testing
      "component"
      (testing "builds a ref to a map containing :eid and :type"
        (let [c1 (cs/component 'my-ent :my-type)]
          (is (= {:eid 'my-ent, :type :my-type} @c1))))

      (testing "builds a ref to a map containing :eid, :type and other specified attributes"
        (let [c1 (cs/component 'my-ent :my-type {:attr1 "my attr1"})]
          (is (= {:eid 'my-ent, :type :my-type, :attr1 "my attr1"} @c1))))
      )

    (testing 
      "add-component and get-components-for-entity"
      (testing "associates a component to the given entity"
        (let [cmp (cs/component 'entity1 :thing)
              cstore1 (cs/add-component cstore cmp)
              comps   (cs/get-components-for-entity cstore1 'entity1 :thing)]
          (is (= 1 (count comps)))
          (is (= (deref cmp) (deref (first comps))))
          ))

      (testing "can associate multiple component types with same entity"
        (let [cmp-x (cs/component 'entity1 :x-type {:name "Mr. X"})
              cmp-y (cs/component 'entity1 :y-type {:name "Ms. Y"})
              cstore1 (cs/add-component cstore cmp-x)
              cstore2 (cs/add-component cstore1 cmp-y)
              x-comps   (cs/get-components-for-entity cstore2 'entity1 :x-type)
              y-comps   (cs/get-components-for-entity cstore2 'entity1 :y-type) ]
          (is (= 1 (count x-comps)))
          (is (= @cmp-x (deref (first x-comps))))
          (is (= 1 (count y-comps)))
          (is (= @cmp-y (deref (first y-comps))))))

      (testing "associates multiple same-typed components to the entity"
        (let [cmp-a   (cs/component 'entity1 :thing {:name "A"})
              cmp-b   (cs/component 'entity1 :thing {:name "B"})
              cstore1 (cs/add-component cstore cmp-a)
              cstore2 (cs/add-component cstore1 cmp-b)
              comps   (cs/get-components-for-entity cstore2 'entity1 :thing) ]
          (is (= 2 (count comps)))
          (is (= @cmp-b (deref (first comps))))
          (is (= @cmp-a (deref (second comps))))))

      (testing "isolates components based on entity"
        (let [
              cmp-1a (cs/component 'entity1 :thing {:name "A"})
              cmp-1b (cs/component 'entity1 :thing {:name "B"})
              cmp-2a (cs/component 'entity2 :thing {:name "A"})
              cmp-2b (cs/component 'entity2 :thing {:name "B"})

              cstore1 (reduce cs/add-component cstore [cmp-1a cmp-1b cmp-2a cmp-2b])

              e1-comps   (cs/get-components-for-entity cstore1 'entity1 :thing)
              e2-comps   (cs/get-components-for-entity cstore1 'entity2 :thing)]
          (is (= 2 (count e1-comps)))
          (is (= @cmp-1b (deref (first e1-comps))))
          (is (= @cmp-1a (deref (second e1-comps))))

          (is (= 2 (count e2-comps)))
          (is (= @cmp-2b (deref (first e2-comps))))
          (is (= @cmp-2a (deref (second e2-comps))))))

    )
    )

    (let [_pairs [['e1 :box   {:name "A"}]
                 ['e1 :tiger {:name "Fred"}]
                 ['e1 :box   {:name "B"}]
                 ['e3 :truck {:name "Mater"}]
                 ['e3 :box   {:name "C"}]
                 ['e4 :box   {:name "D"}]
                 ['e5 :truck {:name "Mack"}]
                 ['e5 :box   {:name "E"}]
                 ['e5 :tiger   {:name "Jameson"}]]
          _cmps  (map #(apply cs/component %1) _pairs)
          cstore (reduce cs/add-component (cs/component-store) _cmps)]
      (testing "get-all-components"
        (testing "finds all components of a give type, independent of entity"
          (let [boxes  (cs/get-all-components cstore :box)
                box-names  (set (map (comp :name deref) boxes))

                trucks (cs/get-all-components cstore :truck)
                truck-names (set (map (comp :name deref) trucks))]

            (is (= box-names #{"A" "B" "C" "D" "E"}))
            (is (= truck-names #{"Mater" "Mack"}))
            ))

        (testing "returns empty list if no components of the given type exist"
          (is (= 0 (count (cs/get-all-components cstore :whatevs))))))

      (testing "map-components"
        (testing "visits 'tuples' of 1 component linked by entity, only when both components are present"
          (let [getem (fn [ctype1] 
                         (set (cs/map-components cstore (fn [a] (:name @a)) ctype1)))]
            (is (= #{"A" "B" "C" "D" "E"} (getem :box)))
            (is (= #{"Fred" "Jameson"} (getem :tiger)))
            (is (= #{} (getem :hog)))
            )
          )

        (testing "visits tuples of 2 components linked by entity, only when both components are present"
          (let [getem2 (fn [ctype1 ctype2]
                         (set (cs/map-components cstore (fn [a b] [(:name @a) (:name @b)]) ctype1 ctype2))) ]
            (is (= #{["E" "Mack"] ["C" "Mater"]} (getem2 :box :truck)))
            (is (= #{["A" "Fred"] ["B" "Fred"] ["E" "Jameson"]} (getem2 :box :tiger)))
            (is (= #{["A" "A"] ["A" "B"] ["B" "A"] ["B" "B"] 
                     ["C" "C"] ["D" "D"] ["E" "E"]} (getem2 :box :box)))
            )
          )

        (testing "visits tuples of 3 components linked by entity"
          (let [getem3 (fn [ctype1 ctype2 ctype3]
                         (set (cs/map-components cstore (fn [a b c] [(:name @a) (:name @b) (:name @c)]) ctype1 ctype2 ctype3)))]
            (is (= #{["E" "Mack" "Jameson"]} (getem3 :box :truck :tiger)))
            )
          )

        )
      )

      (let [_pairs [['e1 :fish    {:name "Trout" :counter 2}]
                    ['e1 :cat {:name "Jameson" :counter 20}]
                    ['e2 :fish    {:name "Salmon" :counter 6}]
                    ['e2 :cat {:name "Fred" :counter 10}]]
            mk-cstore (fn [] (reduce cs/add-component (cs/component-store)
                                     (map #(apply cs/component %1) _pairs)))]
        (testing "update-components"

          (testing "can seek by component type and update that component"
            (let [cstore (mk-cstore)
                  countup (fn [m] (assoc m :counter (inc (:counter m))))]
              (dosync
                (cs/update-components cstore countup :fish))
              (is (= 3 (:counter @(cs/get-component-for-entity cstore 'e1 :fish))))
              (is (= 7 (:counter @(cs/get-component-for-entity cstore 'e2 :fish))))
              (is (= 20 (:counter @(cs/get-component-for-entity cstore 'e1 :cat))))
              (is (= 10 (:counter @(cs/get-component-for-entity cstore 'e2 :cat))))

              (dosync
                (cs/update-components cstore countup :cat))
              (is (= 3 (:counter @(cs/get-component-for-entity cstore 'e1 :fish))))
              (is (= 7 (:counter @(cs/get-component-for-entity cstore 'e2 :fish))))
              (is (= 21 (:counter @(cs/get-component-for-entity cstore 'e1 :cat))))
              (is (= 11 (:counter @(cs/get-component-for-entity cstore 'e2 :cat))))
              ))

          (testing "can seek by multiple component types and update the first component"
            (let [cstore (mk-cstore)
                  eat-fish (fn [cat fish] (assoc cat :counter (+ (:counter cat) (:counter fish))))]
              (dosync
                (cs/update-components cstore eat-fish :cat :fish))
              (is (= 2 (:counter @(cs/get-component-for-entity cstore 'e1 :fish))))
              (is (= 6 (:counter @(cs/get-component-for-entity cstore 'e2 :fish))))
              (is (= 22 (:counter @(cs/get-component-for-entity cstore 'e1 :cat))))
              (is (= 16 (:counter @(cs/get-component-for-entity cstore 'e2 :cat))))
              ))
        )       
  )
); end deftest



