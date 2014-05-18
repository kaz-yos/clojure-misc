(defn collatz
  ([n] (collatz n 0))
  ([n c]
     (cond
      ;; stop conditions
      (< n 1) "Out of range"
      (= n 1) c
      (even? n) (recur (/ n 2)       (inc c))
      :else     (recur (inc (* n 3)) (inc c)))))

(collatz 1)
(collatz 2)
(collatz 3)

(map collatz (range 2 (inc 100)))
(map collatz (range 2 (inc 1000)))
(map collatz (range 2 (inc 10000)))
(last (map collatz (range 2 (inc 10000))))
