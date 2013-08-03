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
      "add-component and get-components"
      (testing "associates a component to the given entity"
        (let [cmp (cs/component 'entity1 :thing)
              cstore1 (cs/add-component cstore cmp)
              comps   (cs/get-components cstore1 'entity1 :thing)]
          (is (= 1 (count comps)))
          (is (= (deref cmp) (deref (first comps))))
          ))

      (testing "can associate multiple component types with same entity"
        (let [cmp-x (cs/component 'entity1 :x-type {:name "Mr. X"})
              cmp-y (cs/component 'entity1 :y-type {:name "Ms. Y"})
              cstore1 (cs/add-component cstore cmp-x)
              cstore2 (cs/add-component cstore1 cmp-y)
              x-comps   (cs/get-components cstore2 'entity1 :x-type)
              y-comps   (cs/get-components cstore2 'entity1 :y-type) ]
          (is (= 1 (count x-comps)))
          (is (= @cmp-x (deref (first x-comps))))
          (is (= 1 (count y-comps)))
          (is (= @cmp-y (deref (first y-comps))))))

      (testing "associates multiple same-typed components to the entity"
        (let [cmp-a   (cs/component 'entity1 :thing {:name "A"})
              cmp-b   (cs/component 'entity1 :thing {:name "B"})
              cstore1 (cs/add-component cstore cmp-a)
              cstore2 (cs/add-component cstore1 cmp-b)
              comps   (cs/get-components cstore2 'entity1 :thing) ]
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

              e1-comps   (cs/get-components cstore1 'entity1 :thing)
              e2-comps   (cs/get-components cstore1 'entity2 :thing)]
          (is (= 2 (count e1-comps)))
          (is (= @cmp-1b (deref (first e1-comps))))
          (is (= @cmp-1a (deref (second e1-comps))))

          (is (= 2 (count e2-comps)))
          (is (= @cmp-2b (deref (first e2-comps))))
          (is (= @cmp-2a (deref (second e2-comps))))))

    )

    (let [_pairs [['e1 :box   {:name "A"}]
                 ['e1 :tiger {:name "Fred"}]
                 ['e1 :box   {:name "B"}]
                 ['e3 :truck {:name "Mater"}]
                 ['e3 :box   {:name "C"}]
                 ['e4 :box   {:name "D"}]
                 ['e5 :truck {:name "Mack"}]
                 ['e5 :box   {:name "E"}]]
          _cmps  (map #(apply cs/component %1) _pairs)
          cstore (reduce cs/add-component cstore _cmps)]
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

      (testing "with-components-linked-by-entity"
        (testing "visits tuples of 2 components linked by entity, only when both components are present"
          (let [captured  (cs/with-components-linked-by-entity cstore [:box :truck] 
                     (fn [box-ref truck-ref] [(:name @box-ref) (:name @truck-ref)]))]

            (is (= #{["E" "Mack"] ["C" "Mater"]} (set captured))))
          )

        (testing "visits tuples of 3 components linked by entity"
          )

        )
      )

      )); end deftest



