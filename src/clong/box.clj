(ns clong.box
  )

(defn to-box 
  "Given a map with :position and :size vectors, 
  return a vector of four values [top left bottom right]"
  [{[x y] :position [w h] :size}]
  [(+ y h) x y (+ x w)])

(defn to-pts [[t l b r]]
  [[l t] [r t] [l b] [r b]])

(defn contains-pt? [[t l b r] [x y]]
  (and (> x l) (< x r) (> y b) (< y t)))

(defn box-piercing-box? [smaller larger]
  (let [pts (to-pts smaller)]
    (some #(contains-pt? larger %1) pts)))
