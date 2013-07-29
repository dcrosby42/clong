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
          (pprint comps)
          (is (= 1 (count comps)))
          (is (= {:type :thing :eid 'entity1} (deref (first comps))))
          ))
    )))

