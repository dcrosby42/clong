(ns clong.ecs.component-store-test
  (:use clojure.test
        clojure.pprint)
  (:require [clong.ecs.component-store :as cs]))

(deftest 
  component-store-test
  (let [mk-cstore (fn [cmps] (let [cstore (cs/component-store)]
                               (dosync (doseq [c cmps] (cs/add-component cstore c)))
                               cstore))
        mk-comps (fn [pairs] (map #(apply cs/component %1) pairs))]

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
        (let [cstore (mk-cstore [])
              cmp (cs/component 'entity1 :thing)
              _ (dosync (cs/add-component cstore cmp))
              comps   (cs/get-components-for-entity cstore 'entity1 :thing)]
          (is (= 1 (count comps)))
          (is (= (deref cmp) (deref (first comps))))
          ))

      (testing "can associate multiple component types with same entity"
        (let [cstore (mk-cstore [])
              cmp-x (cs/component 'entity1 :x-type {:name "Mr. X"})
              cmp-y (cs/component 'entity1 :y-type {:name "Ms. Y"})
              _ (dosync (cs/add-component cstore cmp-x))
              _ (dosync (cs/add-component cstore cmp-y))
              x-comps   (cs/get-components-for-entity cstore 'entity1 :x-type)
              y-comps   (cs/get-components-for-entity cstore 'entity1 :y-type) ]
          (is (= 1 (count x-comps)))
          (is (= @cmp-x (deref (first x-comps))))
          (is (= 1 (count y-comps)))
          (is (= @cmp-y (deref (first y-comps))))))

      (testing "associates multiple same-typed components to the entity"
        (let [cstore (mk-cstore [])
              cmp-a   (cs/component 'entity1 :thing {:name "A"})
              cmp-b   (cs/component 'entity1 :thing {:name "B"})
              _ (dosync (cs/add-component cstore cmp-a))
              _ (dosync (cs/add-component cstore cmp-b))
              comps   (cs/get-components-for-entity cstore 'entity1 :thing)]
          (is (= 2 (count comps)))
          (is (= @cmp-b (deref (first comps))))
          (is (= @cmp-a (deref (second comps))))))

      (testing "isolates components based on entity"
        (let [
              cmp-1a (cs/component 'entity1 :thing {:name "A"})
              cmp-1b (cs/component 'entity1 :thing {:name "B"})
              cmp-2a (cs/component 'entity2 :thing {:name "A"})
              cmp-2b (cs/component 'entity2 :thing {:name "B"})
              cstore (mk-cstore [cmp-1a cmp-1b cmp-2a cmp-2b])

              e1-comps   (cs/get-components-for-entity cstore 'entity1 :thing)
              e2-comps   (cs/get-components-for-entity cstore 'entity2 :thing)]
          (is (= 2 (count e1-comps)))
          (is (= @cmp-1b (deref (first e1-comps))))
          (is (= @cmp-1a (deref (second e1-comps))))

          (is (= 2 (count e2-comps)))
          (is (= @cmp-2b (deref (first e2-comps))))
          (is (= @cmp-2a (deref (second e2-comps))))))

    )
    
    (let [cstore (mk-cstore (mk-comps 
                              [['e1 :box   {:name "A"}]
                               ['e1 :tiger {:name "Fred"}]
                               ['e1 :box   {:name "B"}]
                               ['e2 :boss  {:name "Great Huntress"}]
                               ['e3 :truck {:name "Mater"}]
                               ['e3 :box   {:name "C"}]
                               ['e4 :box   {:name "D"}]
                               ['e5 :truck {:name "Mack"}]
                               ['e5 :box   {:name "E"}]
                               ['e5 :tiger   {:name "Jameson"}]
                               ['w  :map   {:name "The Map"}]]) )]

      (testing "get-components"

        (testing "finds all components of a give type, independent of entity"
          (let [boxes  (cs/get-components cstore :box)
                box-names  (set (map (comp :name deref) boxes))

                trucks (cs/get-components cstore :truck)
                truck-names (set (map (comp :name deref) trucks))]

            (is (= box-names #{"A" "B" "C" "D" "E"}))
            (is (= truck-names #{"Mater" "Mack"}))
            ))

        (testing "returns empty list if no components of the given type exist"
          (is (= 0 (count (cs/get-components cstore :whatevs))))))

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
            (is (= #{["A" "B"] ["B" "A"]} (getem2 :box :box)))
            )
          )

        (testing "visits tuples of 3 components linked by entity"
          (let [getem3 (fn [ctype1 ctype2 ctype3]
                         (set (cs/map-components cstore (fn [a b c] [(:name @a) (:name @b) (:name @c)]) ctype1 ctype2 ctype3)))]
            (is (= #{["E" "Mack" "Jameson"]} (getem3 :box :truck :tiger)))
            ))

        (testing "does not combine components with themselves" 
          ;; ...but currently still does full permutation beyond that.
          ;; IDEALLY I'd like ["A" "B"] to preclude ["B" "A"], that is, to reduce positional symmetry 
          (let [cstore (mk-cstore (mk-comps 
                                      [['e1 :card   {:name "Visa"}]
                                       ['e1 :card   {:name "Master"}]
                                       ['e1 :card   {:name "Amex"}]]))
                  getem3 (fn [ctype1 ctype2 ctype3]
                           (set (cs/map-components cstore (fn [a b c] [(:name @a) (:name @b) (:name @c)]) ctype1 ctype2 ctype3)))
                  getem2 (fn [ctype1 ctype2]
                           (set (cs/map-components cstore (fn [a b] [(:name @a) (:name @b)]) ctype1 ctype2))) 
                  ]
              (is (= #{["Visa" "Master" "Amex"] 
                       ["Visa" "Amex" "Master"]
                       ["Amex" "Master" "Visa"]
                       ["Amex" "Visa" "Master"]
                       ["Master" "Visa" "Amex"]
                       ["Master" "Amex" "Visa"]} (getem3 :card :card :card)))

              (is (= #{["Visa" "Master"] 
                       ["Visa" "Amex"]
                       ["Amex" "Master"]
                       ["Amex" "Visa"]
                       ["Master" "Visa"]
                       ["Master" "Amex"]} (getem2 :card :card)))

              ))
        )

        (testing "map-components'"
          (testing "it can perform one component search"
            (let [truck-result (cs/map-components' cstore (fn [[truck]] (:name @truck)) [:truck])
                  tiger-result (cs/map-components' cstore (fn [[tiger]] (:name @tiger)) [:tiger])]
              (is (= 2 (count truck-result)))
              (is (= #{"Mater" "Mack"} (set truck-result)))
              (is (= 2 (count tiger-result)))
              (is (= #{"Fred" "Jameson"} (set tiger-result)))))

          (testing "it can combine two component searches"
            (let [truck-result (cs/map-components' cstore 
                                                   (fn [[the-map] [truck box]] (str (:name @truck) " " (:name @box) " is on " (:name @the-map)))
                                                   [:map] [:truck :box])]
              (is (= 2 (count truck-result)))
              (is (= #{"Mack E is on The Map", "Mater C is on The Map"} (set truck-result)))
              ))

          (testing "it can combine two component searches"
            (let [truck-result (cs/map-components' cstore 
                                                   (fn [[the-map] [truck box]] (str (:name @truck) " " (:name @box) " is on " (:name @the-map)))
                                                   [:map] [:truck :box])]
              (is (= 2 (count truck-result)))
              (is (= #{"Mack E is on The Map", "Mater C is on The Map"} (set truck-result)))
              ))

          (testing "it can combine three component searches"
            (let [truck-result (cs/map-components' cstore 
                                                   (fn [[the-map] [truck box] [boss]] (str (:name @truck) " " (:name @box) " is on " (:name @the-map) " and the boss is " (:name @boss)))
                                                   [:map] [:truck :box] [:boss])]
              (is (= 2 (count truck-result)))
              (is (= #{"Mack E is on The Map and the boss is Great Huntress", "Mater C is on The Map and the boss is Great Huntress"} (set truck-result)))
              ))
          )

      )

        (testing "remove-component"
          (let [cstore (cs/component-store)
                left-gun   (cs/component 'me :gun {:style "Remington"})
                right-gun  (cs/component 'me :gun {:style "Colt"})
                other-gun  (cs/component 'other :gun {:style "Colt"})
                peek-guns (fn [eid] (set (map 
                                           (fn [gun-ref] (:style @gun-ref)) 
                                           (cs/get-components-for-entity cstore eid :gun))))
                ]
            (dosync
                  (cs/add-component cstore left-gun)
                  (cs/add-component cstore right-gun)
                  (cs/add-component cstore other-gun)
              )
            ; sanity check:
            (is (= #{"Remington" "Colt"} (peek-guns 'me)))
            ; go:
            (dosync (cs/remove-component cstore left-gun))
            ; see remington is gone:
            (is (= #{"Colt"} (peek-guns 'me)))
            ; see other entity unnaffected:
            (is (= #{"Colt"} (peek-guns 'other)))

            )

          )
          

      (let [mk-kittehs (fn [] 
                         (mk-cstore (mk-comps
                                [['e1 :fish    {:name "Trout" :counter 2}]
                                 ['e1 :cat {:name "Jameson" :counter 20}]
                                 ['e2 :fish    {:name "Salmon" :counter 6}]
                                 ['e2 :cat {:name "Fred" :counter 10}]])))]

            (testing "update-components"

              (testing "can seek by component type and update that component"
                (let [cstore (mk-kittehs)
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
                (let [cstore (mk-kittehs)
                      eat-fish (fn [cat fish] (assoc cat :counter (+ (:counter cat) (:counter fish))))]
                  (dosync
                    (cs/update-components cstore eat-fish :cat :fish))
                  (is (= 2 (:counter @(cs/get-component-for-entity cstore 'e1 :fish))))
                  (is (= 6 (:counter @(cs/get-component-for-entity cstore 'e2 :fish))))
                  (is (= 22 (:counter @(cs/get-component-for-entity cstore 'e1 :cat))))

                  (is (= 16 (:counter @(cs/get-component-for-entity cstore 'e2 :cat))))
                  ))

              ) ; update-components       
            ) ; let

            ); outter let
) ;deftest



