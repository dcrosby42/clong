(dosync
        (alter a-ref (fn [a] (inc a)))
        (alter b-ref (fn [b] (inc b)))
        (alter c-ref (fn [c] (inc c))))

(dosync (alter a-ref (fn [a]
                       (alter b-ref (fn [b] 
                                      (alter c-ref (fn [c]
                                                     (inc c)))
                                      (inc b)))
                       (inc a))))
  
(udpate [a 
