;;; 2014-04-10 Clojure Meetup Review

;;; Q 15
;; 4Clojure Question 15
;;
;; Write a function which doubles a number.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [x] (* x 2))

(= (__ 2) 4)
(= (__ 3) 6)
(= (__ 11) 22)
(= (__ 7) 14)



;;; Q 33

;; 4Clojure Question 33
;;
;; Write a function which replicates each element of a sequence a variable number of times.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [x n]
  (mapcat (partial take n) (map repeat x)))
;; (partial take n) creates a new function with fewer arguments

(defn __ [x n]
  (mapcat (partial repeat n) x))
;; repeat can take a number of repeat argument as the first argument

(= (__ [1 2 3] 2) '(1 1 2 2 3 3))
(= (__ [:a :b] 4) '(:a :a :a :a :b :b :b :b))
(= (__ [4 5 6] 1) '(4 5 6))
(= (__ [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
(= (__ [44 33] 2) [44 44 33 33])



;;; Q 38

;; 4Clojure Question 38
;;
;; Write a function which takes a variable number of parameters and returns the maximum value.
;;
;; Restrictions (please don't use these function(s)): max, max-key
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [& rest]
  (reduce (fn [x y]
            (if (>= x y)
              x
              y))
          rest))
;; rest is already a list

(= (__ 1 8 3 4) 8)
(= (__ 30 20) 30)
(= (__ 45 67 11) 67)


;;; Q 77

;; 4Clojure Question 77
;;
;; Write a function which finds all the anagrams in a vector of words.  A word x is an anagram of word y if all the letters in x can be rearranged in a different order to form y.  Your function should return a set of sets, where each sub-set is a group of words which are anagrams of each other.  Each sub-set should have at least two words.  Words without any anagrams should not be included in the result.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [x]
  (set
   (map set
        (filter
         (fn [el]
           (> (count el) 1))
         (vals (group-by sort x))))))

;; Give filter a function that returns true/false, it does something like x[count(x) > 1].


;; (group-by fun sequence) will apply the fun to each element of the sequence
;; and group by the result as keys. An array map is returned.
;; {key val key val key val}

(= (__ ["meat" "mat" "team" "mate" "eat"])
   #{#{"meat" "team" "mate"}})
(= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
   #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})


;;; Q110

;; 4Clojure Question 110
;;
;; <p>Write a function that returns a lazy sequence of "pronunciations" of a sequence of numbers. A pronunciation of each element in the sequence consists of the number of repeating identical numbers and the number itself. For example, <code>[1 1]</code> is pronounced as <code>[2 1]</code> ("two ones"), which in turn is pronounced as <code>[1 2 1 1]</code> ("one two, one one").</p><p>Your function should accept an initial sequence of numbers, and return an infinite lazy sequence of pronunciations, each element being a pronunciation of the previous element.</p>
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [])

(= [[1 1] [2 1] [1 2 1 1]] (take 3 (__ [1])))
(= [3 1 2 4] (first (__ [1 1 1 4 4])))
(= [1 1 1 3 2 1 3 2 1 1] (nth (__ [1]) 6))
(= 338 (count (nth (__ [3 2]) 15)))

(def pack (partial partition-by identity))

(pack [1 1 1 3 2 1 3 2 1 1])

(map count (pack [1 1 1 3 2 1 3 2 1 1]))

(apply concat (map distinct (pack [1 1 1 3 2 1 3 2 1 1])))

(interleave (map count (pack [1 1 1 3 2 1 3 2 1 1]))
            (apply concat (map distinct (pack [1 1 1 3 2 1 3 2 1 1]))))

(interleave (map count (pack [1 1]))
            (apply concat (map distinct (pack [1 1]))))

(defn my-seq [seed]
  (let [pack (partial partition-by identity)]
    (interleave (map count (pack seed))
                (apply concat (map distinct (pack seed))))))

(my-seq [1])
(my-seq [1 1])
(my-seq [2 1])
(my-seq [1 2 1 1])

(take 3 (iterate my-seq [1]))





(defn my-lazy-seq [seed]
  (let [pack (partial partition-by identity)
        my-seq (fn [seed]
                 interleave (map count (pack seed))
                 (apply concat (map distinct (pack seed))))]
    (my-seq seed)))

(take 3 (my-lazy-seq [1]))
;; Returns a lazy sequence of x, (f x), (f (f x)) etc. f must be free of side-effects


;; Recursion is a low-level operation
;; http://blog.fogus.me/2011/03/09/recursion-is-a-low-level-operation/
