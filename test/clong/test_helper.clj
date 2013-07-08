(ns clong.test-helper
  (:use clojure.test)
  )

;(defonce last-run-tests 
(defn derive-source-ns [sym]
  (let [s (str sym)]
    (symbol (.substring s 0 (.lastIndexOf s "-test")))))

(defn rt [tns]
  (use 'clong.test-helper :reload)
  (require (derive-source-ns tns) :reload)
  (require tns :reload)
  (let [result (run-tests tns)]
    (println result)
    (if (= 0 (+ (:error result) (:fail result)))
      'OK
      'FAIL)))

