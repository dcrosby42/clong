(ns clong.ecs.entity-manager-test
  (:use clojure.test)
  (:require [clong.ecs.entity-manager :as em]))

(deftest 
  entity-manager-test
  (let [manager (em/manager)]
    (testing 
      "manager"
      (is (= {} manager)))

    (testing 
      "add-entity"
      (testing "puts a new entity into the manager"
               (let [m1       (em/add-entity manager :foo ["foo data"] :bar {:x 42})
                     eid      (first (keys m1))
                     ent-ref  (get m1 eid)
                     entity   @ent-ref]
                 (is (= {:eid eid, :foo ["foo data"] :bar {:x 42}} entity))))

      (testing "uses a different entity id each time"
               (let [m1       (em/add-entity manager :laser [])
                     m2       (em/add-entity m1 :laser [])
                     ks       (keys m2)
                     vs       (map deref (vals m2))]
                 (is (= (count (set ks)) 2))
                 (is (= (set [{:eid (first ks), :laser []} {:eid (second ks), :laser []}]) 
                        (set vs)))))
      )

    (testing
      "remove-entity"
      (let [m1 (em/add-entity manager :laser [])]
        (testing "removes an entity from the manager"
                 (let [eid (first (keys m1))]
                   (is (= 1 (count (keys m1))))
                   (let [m2 (em/remove-entity m1 eid)]
                     (is (= 0 (count (keys m2)))))))

        (testing "doesn't disturb other entities in the manager"
                 (let [eid (first (keys m1))
                       m2 (em/add-entity m1 :bomb [])
                       m3 (em/add-entity m2 :health [])]
                   (is (= 3 (count (keys m3))))
                   (let [m4 (em/remove-entity m3 eid)
                         ents (map deref (vals m4))]
                     (is (= 2 (count ents)))
                     (is (some #(contains? %1 :bomb) ents))
                     (is (some #(contains? %1 :health) ents)))))
        ))

    (testing
      "remove-entities"
      (let [m1 (-> manager
                 (em/add-entity :laser []) 
                 (em/add-entity :bomb []))
            initial-eids (keys m1)]
        (is (= 2 (count initial-eids)))
        (testing 
          "removes a series of entities from the manager according to the given seq of entity ids"
          (let [m2 (em/add-entity m1 :health 10)
                m3 (em/remove-entities m2 initial-eids)]
            (is (= 1 (count (keys m3))))
            (is (contains? (deref (first (vals m3))) :health))))
        ))






    ) ; outer let
  ) ; deftest





