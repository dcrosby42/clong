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
              cstore1 (cs/add-component cstore cmp-1a)
              cstore2 (cs/add-component cstore1 cmp-1b)
              cstore3 (cs/add-component cstore2 cmp-2a)
              cstore4 (cs/add-component cstore3 cmp-2b)
              e1-comps   (cs/get-components cstore4 'entity1 :thing)
              e2-comps   (cs/get-components cstore4 'entity2 :thing)]
          (is (= 2 (count e1-comps)))
          (is (= @cmp-1b (deref (first e1-comps))))
          (is (= @cmp-1a (deref (second e1-comps))))

          (is (= 2 (count e2-comps)))
          (is (= @cmp-2b (deref (first e2-comps))))
          (is (= @cmp-2a (deref (second e2-comps))))))

    )

    ; (testing "all-components"
    ;   (testing "finds all components of ctype, independent of entity"
    ;     (let [pairs [['e1 {:type :box :name "A"}]
    ;                  ['e1 {:type :tiger :name "Fred"}]
    ;                  ['e1 {:type :box :name "B"}]
    ;                  ['e2 {:type :truck :name "Mater"}]
    ;                  ['e3 {:type :box :name "C"}]
    ;                  ['e4 {:type :box :name "D"}]]
    ;           (reduce (fn [cs [eid c]]  


    )); end deftest

