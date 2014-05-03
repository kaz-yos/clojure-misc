(defn oddsum-1 [n]
  (->> (range (inc n))
       (filter odd?)
       (reduce +)))
(oddsum-1 1000)

(defn oddsum-2 [n]
  (loop [i 1
         s 0]
    (if (> i n)
      s              ; return accumulator if
      (recur (+ 2 i) ; index 1, 3, 5, ...
             (+ s i) ; accumulator add index at each point
             ))
    ))
(oddsum-2 1000)

(defn oddsum-3 [n]
  (->> (iterate #(+ 2 %) 1)
       (filter #(< % n))
       (reduce +)))
(oddsum-3 1000)
