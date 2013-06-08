(ns clong.utils
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GENERAL UTILS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn vmap [f m] 
  (apply array-map (flatten (map (fn [[k v]] [k (f k v)]) m))))

(defn clamp [lo hi v] 
  (if (< v lo) lo (if (> v hi) hi v)))

(def reject-nils (partial keep identity))

(defn btw [lo hi x] (and (>= x lo) (<= x hi)))

(defn find-by [maps k v] (first (filter #(= v (get %1 k)) maps)))

(defn bool-to-int [b] (if b 1 0))
