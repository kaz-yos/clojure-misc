;;; 2014-04-10 Clojure meet up

#(* %1 %2)
#(inc %)
(fn [x y] (* x y))

;; #() Cannot be nested


;; 4Clojure Question 15
;;
;; Write a function which doubles a number.
;;
;; Use M-x 4clojure-check-answers when you're done!

(= ((fn [x] (* x 2)) 2) 4)

(= ((fn [x] (* x 2)) 3) 6)

(= ((fn [x] (* x 2)) 11) 22)

(= (#'(+ x x) 7) 14)


;; 4Clojure Question 33
;;
;; Write a function which replicates each element of a sequence a variable number of times.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [x n]
  (apply concat (map (fn [ele] (take n (repeat ele))) x)))

(defn __ [x n]
  (mapcat (fn [ele] (take n (repeat ele))) x))

(= (__ [1 2 3] 2) '(1 1 2 2 3 3))

(= (__ [:a :b] 4) '(:a :a :a :a :b :b :b :b))

(= (__ [4 5 6] 1) '(4 5 6))

(= (__ [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))

(= (__ [44 33] 2) [44 44 33 33])



;; 4Clojure Question 38
;;
;; Write a function which takes a variable number of parameters and returns the maximum value.
;;
;; Restrictions (please don't use these function(s)): max, max-key
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [& rest]
  (reduce (fn [x y] (if (<= x y) y x)) rest))   ; rest is a sequence

#(last (sort %&))

(= (__ 1 8 3 4) 8)

(= (__ 30 20) 30)

(= (__ 45 67 11) 67)


(macroexpand '#(last (sort %&)))

(comp last sort list)




;; 4Clojure Question 77
;;
;; Write a function which finds all the anagrams in a vector of words.  A word x is an anagram of word y if all the letters in x can be rearranged in a different order to form y.  Your function should return a set of sets, where each sub-set is a group of words which are anagrams of each other.  Each sub-set should have at least two words.  Words without any anagrams should not be included in the result.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [coll]
  (set (map set (filter (fn [x] (> (count x) 1)) (map second (group-by sort coll))))))

;; group-by

(= (__ ["meat" "mat" "team" "mate" "eat"])
   #{#{"meat" "team" "mate"}})
(= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
   #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})


#(->> % (group-by sort) vals (filter next) (map set) set)



;; 4Clojure Question 102
;;
;; When working with java, you often need to create an object with <code>fieldsLikeThis</code>, but you'd rather work with a hashmap that has <code>:keys-like-this</code> until it's time to convert. Write a function which takes lower-case hyphen-separated strings and converts them to camel-case strings.
;;
;; Use M-x 4clojure-check-answers when you're done!

(= (__ "something") "something")
(= (__ "multi-word-key") "multiWordKey")
(= (__ "leaveMeAlone") "leaveMeAlone")




;; 4Clojure Question 110
;;
;; <p>Write a function that returns a lazy sequence of "pronunciations" of a sequence of numbers. A pronunciation of each element in the sequence consists of the number of repeating identical numbers and the number itself. For example, <code>[1 1]</code> is pronounced as <code>[2 1]</code> ("two ones"), which in turn is pronounced as <code>[1 2 1 1]</code> ("one two, one one").</p><p>Your function should accept an initial sequence of numbers, and return an infinite lazy sequence of pronunciations, each element being a pronunciation of the previous element.</p>
;;
;; Use M-x 4clojure-check-answers when you're done!

(= [[1 1] [2 1] [1 2 1 1]] (take 3 (__ [1])))

(= [3 1 2 4] (first (__ [1 1 1 4 4])))

(= [1 1 1 3 2 1 3 2 1 1] (nth (__ [1]) 6))

(= 338 (count (nth (__ [3 2]) 15)))


(partition-by identity [1 1 1 3 3 2 2 2 2 2 2])


;; http://blog.fogus.me/2011/03/09/recursion-is-a-low-level-operation/
(def S '[a a a a b c c a a d e e e e])

(def pack (partial partition-by identity))

(pack S)
;;=> ((a a a a) (b) (c c) (a a) (d) (e e e e))

(def rle (comp (partial map (juxt first count)) pack))

(rle S)
;=> ([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])

(def rld (partial mapcat (partial apply repeat)))

(-> S rle rld)
;;=> (a a a a b c c a a d e e e e)


(interleave [1 2 3] [9 8 7])
