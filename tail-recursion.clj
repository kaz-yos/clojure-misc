;;; tail recursion examples
;; factorial
(defn fact
  ;; body 1 for one argument special case
  ([curr]
     (if (= curr 0)
       1
       (recur (dec curr) curr)))
  ;; body 2 for 2 argument general case
  ([curr acc]
     (if (= curr 0)
       acc
       (recur (dec curr) (*' acc curr)))))

(fact 0)
(fact 1)
(fact 10)
(fact 100)
(fact 200)


(map fact (range 0 11))
(map fact (range 0 21))
(map fact (range 0 51))
(map fact (range 0 101))

;; R
;; > factorial(20)
;; [1] 2.432902e+18
;; > factorial(50)
;; [1] 3.041409e+64
;; > factorial(100)
;; [1] 9.332622e+157
