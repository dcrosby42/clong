(ns clong.ecs.component-store-test
  (:use clojure.test
        clojure.pprint)
  (:require [clong.ecs.component-store :as cs]))

(deftest 
  component-store-test
  (let [cstore (cs/component-store)]
    (testing 
      "add-component"
      (testing "associates a component to the given entity"
        (let [cstore1 (cs/add-component cstore 'entity1 {:type :thing})
              comps   (cs/get-components cstore1 'entity1 :thing)
              ]
          (is (= 1 (count comps)))
          (is (= {:type :thing :eid 'entity1} (deref (first comps))))
          ))

      (testing "can associate multiple component types with same entity"
        (let [cstore1 (cs/add-component cstore 'entity1 {:type :x, :name "A"})
              cstore2 (cs/add-component cstore1 'entity1 {:type :y, :name "B"})
              x-comps   (cs/get-components cstore2 'entity1 :x)
              y-comps   (cs/get-components cstore2 'entity1 :y) ]
          (is (= 1 (count x-comps)))
          (is (= {:type :x :name "A" :eid 'entity1} (deref (first x-comps))))
          (is (= 1 (count y-comps)))
          (is (= {:type :y :name "B" :eid 'entity1} (deref (first y-comps))))))

      (testing "associates multiple same-typed components to the entity"
        (let [cstore1 (cs/add-component cstore 'entity1 {:type :thing, :name "A"})
              cstore2 (cs/add-component cstore1 'entity1 {:type :thing, :name "B"})
              comps   (cs/get-components cstore2 'entity1 :thing) ]
          (is (= 2 (count comps)))
          (is (= {:type :thing :name "B" :eid 'entity1} (deref (first comps))))
          (is (= {:type :thing :name "A" :eid 'entity1} (deref (second comps))))))

      (testing "isolates components based on entity"
        (let [cstore1 (cs/add-component cstore 'entity1 {:type :thing, :name "A"})
              cstore2 (cs/add-component cstore1 'entity1 {:type :thing, :name "B"})
              cstore3 (cs/add-component cstore2 'entity2 {:type :thing, :name "A"})
              cstore4 (cs/add-component cstore3 'entity2 {:type :thing, :name "B"})
              e1-comps   (cs/get-components cstore4 'entity1 :thing)
              e2-comps   (cs/get-components cstore4 'entity2 :thing)]
          (is (= 2 (count e1-comps)))
          (is (= {:type :thing :name "B" :eid 'entity1} (deref (first e1-comps))))
          (is (= {:type :thing :name "A" :eid 'entity1} (deref (second e1-comps))))

          (is (= 2 (count e2-comps)))
          (is (= {:type :thing :name "B" :eid 'entity2} (deref (first e2-comps))))
          (is (= {:type :thing :name "A" :eid 'entity2} (deref (second e2-comps))))))
    )))

