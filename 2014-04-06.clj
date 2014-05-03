;; test
(clojure-version)

(class (/ 1 3))
(+ 1 2)
(mod 5 4)


(< 1 2 3)
(< 1 5 2 3 4)

(println "master yoda\nluke skywarker\ndarth veder")

;; Conversion to a string
(str 1)
;; Also like paste0() in R
(str 1 2)



(let [x 5]
  (= :your-road
     (cond (= x 1) :road-not-taken
           (= x 2) :another-road-not-taken
           :else :your-road)))



#{1 2 3 4}
(coj #{1 4 3} #{2})

(= 8 ((fn [x] (+ x 5))
      3))

((fn my-last [seq]
   (let [length (count seq)]
     (nth seq (- length 1)))) '(1 2 3))

(my-last '(1 2 3 4 5))

(defn my-last2 [x]
  ((comp first reverse) x))

(my-last2 '(1 2 3 4 5 6))

(fn [seq1]
  (let [length (count seq1)]
    (nth seq1 (- length 2))))

;; my-nth with recursion
(defn my-nth [seq n]
  (if (= n 0)
    (first seq)
    (my-nth (rest seq) (- n 1))))

(defn my-nth [seq n]
  (if (= n 0)
    (first seq)
    (recur (rest seq) (- n 1))))

(my-nth '(1 2 3 4 5) 0)
(my-nth '(1 2 3 4 5) 1)
(my-nth '(1 2 3 4 5) 2)


;; my-count with recursion
(defn my-count [seq]
  (let [count 0]
    (if (= seq '())
      count
      (progn (def count (+ count 1))
             (recur (rest seq))))))

(defn my-count [seq]
  (loop [count 0]
    (if (= '() seq)
      0
      (def seq (rest seq))
      (recur ((+ count 1))))))

(defn fib-even-sum [upto]
  (loop [previous 1 nxt 1 sum 0]
    (if (or (<= upto 1) (>= nxt upto))
     sum)
    (if (= (mod nxt 2) 0)
       (recur nxt (+ previous nxt) (+ sum nxt))
       (recur nxt (+ previous nxt) sum))))

;;
;;
