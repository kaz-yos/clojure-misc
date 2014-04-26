;;; https://www.4clojure.com

;;; Intro to vectors
(= [:a :b :c] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c)) 


;;; https://www.4clojure.com/problem/71
;;; -> macro for rearranging functions in reverse order
(=
 ;; usual method
 (last (sort (rest (reverse [2 5 4 1 3 6]))))
 ;; unix pipeline-like method (like dplyr's %.%)
 (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (last))
 ;;
 5)


;; 4Clojure Question 29
;;
;; Write a function which takes a string and returns a new string containing only the capital letters.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [st]
  (clojure.string/replace st #"[a-z,0-9,$#*&() !]" ""))

;; This is NOT A-Z
(defn __ [st]
  (clojure.string/replace st #"[^A-Z]" ""))

(= (__ "HeLlO, WoRlD!") "HLOWRD")
(empty? (__ "nothing"))
(= (__ "$#A(*&987Zf") "AZ")

;; re-seq is nice
;; clojure.core/re-seq
;; ([re s])
;;   Returns a lazy sequence of successive matches of pattern in string,
;;   using java.util.regex.Matcher.find(), each such match processed with
;;   re-groups.
(re-seq #"[A-Z]" "Kazuki Yoshida")
(apply str (re-seq #"[A-Z]" "Kazuki Yoshida"))
;; ^ means NOT
(re-seq #"[^A-Z]" "Kazuki Yoshida")

;; as is and lower case compared. Non matching filtered in.
(filter #(not= (.toLowerCase (str %)) (str %))
        "Kazuki Yoshida")

(filter #(= (.toUpperCase (str %)) (str %))
        "Kazuki Yoshida")



;; 4Clojure Question 32
;;
;; Write a function which duplicates each element of a sequence.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [seq]
  (mapcat (partial repeat 2) seq))

(defn __ [seq]
  (interleave seq seq))
(interleave [1 2 3] [1 2 3])

(= (__ [1 2 3]) '(1 1 2 2 3 3))
(= (__ [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
(= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
(= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))


(#(interleave % %) [1 2 3])



;; 4Clojure Question 48
;;
;; The some function takes a predicate function and a collection.  It returns the first logical true value of (predicate x) where x is an item in the collection.
;;
;; Use M-x 4clojure-check-answers when you're done!
(some #{2 7 6} [5 6 7 8])

;; Using a set 
(= __ (some #{2 7 6} [5 6 7 8]))
(= __ (some #(when (even? %) %) [5 6 7 8]))




;; 4Clojure Question 34
;;
;; Write a function which creates a list of all integers in a given range.
;;
;; Restrictions (please don't use these function(s)): range
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [start end]
  (if (= start (dec end))
    start
    (flatten [start (__ (inc start) end)])))

(defn __ [start end]
  (if (= start end)
    '()
    (cons start (__ (inc start) end))))

;; by infinite sequence created by iterate
(defn __ [start end]
  (take (- end start) (iterate (fn [x] (inc x)) start)))

(= (__ 1 4) '(1 2 3))
(= (__ -2 2) '(-2 -1 0 1))
(= (__ 5 8) '(5 6 7))





;; 4Clojure Question 28
;;
;; Write a function which flattens a sequence.
;;
;; Restrictions (please don't use these function(s)): flatten
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [s]
  ;; If it is an element
  (if (not (or (list? s) (vector? s)))
    ;; Return itself as a list, necessary for mapcat to work
    (list s)
    ;; If it is a collection, map itself to elements.
    (mapcat __ s))
  )

(defn __ [s]
  ;; If it is an element (not a collection)
  (if (not (coll? s))
    ;; Return itself as a list, necessary for mapcat to work
    (list s)
    ;; If it is a collection, map itself to elements.
    (mapcat __ s))
  )

(defn __ [s] (if (not (coll? s)) [s] (mapcat __ s)))

(defn __ [s] (if (coll? s) (mapcat __ s) [s]))

(= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
(= (__ ["a" ["b"] "c"]) '("a" "b" "c"))
(= (__ '((((:a))))) '(:a))

;; mapcat: map a function, concatenate results
(mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]])
;; (0 1 2 3 4 5 6 7 8 9)




;; 4Clojure Question 42
;;
;; Write a function which calculates factorials.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [n]
  (if (zero? n)
    1
    (* n (__ (dec n)))))

(defn __ [n] (if (zero? n) 1 (* n (__ (dec n)))))

(= (__ 1) 1)
(= (__ 3) 6)
(= (__ 5) 120)
(= (__ 8) 40320)

;; Create a range and apply * to multiply them all
(#(apply * (range 2 (+ 1 %))) 1)





;; 4Clojure Question 39
;;
;; Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc.
;;
;; Restrictions (please don't use these function(s)): interleave
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [a b]
  ;; Stop if either is empty, 
  (if (= 0 (min (count a) (count b)))
    ;; and return an empty vector
    []
    ;; Otherwise recurse
    (flatten [(first a) (first b) (__ (rest a) (rest b))])))

(defn __ [a b]
  (if (or (empty? a) (empty? b))
    []
    (flatten [(first a) (first b) (__ (rest a) (rest b))])))

;; aceeca1
(defn __ [a b]
  (mapcat #(list %1 %2) a b))

(= (__ [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
(= (__ [1 2] [3 4 5 6]) '(1 3 2 4))
(= (__ [1 2 3 4] [5]) [1 5])
(= (__ [30 20] [25 15]) [30 25 20 15])

(mapcat #(list %1 %2) [30 20] [25 15 1 3 ])
(mapcat list [30 20] [25 15 1 3 ])
(map list [30 20] [25 15 1 3 ])



;; 4Clojure Question 30
;;
;; Write a function which removes consecutive duplicates from a sequence.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [s]
  (mapcat distinct (partition-by identity s)))

(= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
(= (__ [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
(= (__ [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))

(partition-by identity "Leeeeeerrroyyy")
(partition-by identity [1 1 2 3 3 2 2 3])
(group-by identity [1 1 2 3 3 2 2 3])   ; group-by does not work here.
(mapcat vals (group-by identity "Leeeeeerrroyyy"))




;; 4Clojure Question 47
;;
;; The contains? function checks if a KEY is present in a given collection.  This often leads beginner clojurians to use it incorrectly with numerically indexed collections like vectors and lists.
;;
;; Use M-x 4clojure-check-answers when you're done!

(def __ 4)
(contains? #{4 5 6} __)         ; Hash set. a key is value itself
(contains? [1 1 1 1 1] __)      ; for a vector key 4 is just position 5
(contains? {4 :a 2 :b} __)      ; map. keys are keys
(not (contains? '(1 2 4) __))



;; 4Clojure Question 45
;;
;; The iterate function can be used to produce an infinite lazy sequence.
;;
;; Use M-x 4clojure-check-answers when you're done!

(= __ (take 5 (iterate #(+ 3 %) 1)))
[1 4 7 10 13]



;; 4Clojure Question 40
;;
;; Write a function which separates the items of a sequence by an arbitrary value.
;;
;; Restrictions (please don't use these function(s)): interpose
;;
;; Use M-x 4clojure-check-answers when you're done!

;; list the interposing value and each element of the collection.
;; Then mapcat to create a one-level list.
;; The first element will be the interposing value, so remove by (rest)
(defn __ [x s]
  (rest (mapcat #(list x %) s)))

(defn __ [x s]
  (butlast (interleave s (repeat x))))

(= (__ 0 [1 2 3]) [1 0 2 0 3])
(= (apply str (__ ", " ["one" "two" "three"])) "one, two, three")
(= (__ :z [:a :b :c :d]) [:a :z :b :z :c :z :d])




;; 4Clojure Question 31
;;
;; Write a function which packs consecutive duplicates into sub-lists.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [s]
  (partition-by identity s))

(= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
(= (__ [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
(= (__ [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))



;; 4Clojure Question 41
;;
;; Write a function which drops every Nth item from a sequence.
;;
;; Use M-x 4clojure-check-answers when you're done!

(butlast (take 3 [1 2 3 4 5 6 7 8]))

(reverse (take (- 8 3) (reverse [1 2 3 4 5 6 7 8])))

(defn __ [s x]
  (let [length (count s)]
   (if (< length x)
     s
     [(butlast (take x s))
      (__ (reverse (take (- length x) (reverse s))) x)])))

(defn __ [s x]
  (let [length (count s)]
    (if (< length x)
      s
      (flatten (list (butlast (take x s))
                     (__ (reverse (take (- length x) (reverse s))) x))))))

;; Take (x - 1) elements work on the rest recursively
(defn __ [s x]
  (let [length (count s)]
    (if (< length x)
      s
      (flatten (list (take (dec x) s)
                     (__ (reverse (take (- length x) (reverse s))) x))))))

;; opposite of take is drop
(defn __ [s x]
  (let [length (count s)]
    (if (< length x)
      s
      (flatten (list (take (dec x) s)
                     (__ (drop x s) x))))))

(__ [] 1)
(__ [1 2 3] 2)

(partition-all 3 [1 2 3 4 5 6 7 8])

;; Partition all in partitions of 3
(defn __ [s x]
  (mapcat #(take (- x 1) %) (partition-all x s)))
(defn __ [s x]
  (mapcat #(take (dec x) %) (partition-all x s)))

(defn __ [coll n]
  (->> (partition-all n coll)
       (map (partial take (dec n)))
       (flatten)))

(defn __ [coll n]
  (->> (partition-all n coll)   ; first step
       (mapcat (partial take (dec n)))))        ; result is then fed to this as a last argument

(= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
(= (__ [:a :b :c :d :e :f] 2) [:a :c :e])
(= (__ [1 2 3 4 5 6] 4) [1 2 3 5 6])



;; 4Clojure Question 52
;;
;; Let bindings and function parameter lists support destructuring.
;;
;; Use M-x 4clojure-check-answers when you're done!

(= [2 4] (let [[a b c d e f g] (range)]
           (vector c e)))


;; 4Clojure Question 49
;;
;; Write a function which will split a sequence into two parts.
;;
;; Restrictions (please don't use these function(s)): split-at
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [n s]
  [(take n s) (reverse (take (- (count s) n) (reverse s)))])

(drop 3 [1 2 3 4 5])


(= (__ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
(= (__ 1 [:a :b :c :d]) [[:a] [:b :c :d]])
(= (__ 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])




;; 4Clojure Question 51
;;
;; Here is an example of some more sophisticated destructuring.
;;
;; Use M-x 4clojure-check-answers when you're done!

(def __ [1 2 3 4 5])

(= [1 2 [3 4 5] [1 2 3 4 5]]
   ;; a 1; b 2; rest as c; whole as d
   (let [[a b & c :as d] __]
     [a b c d]))
;; a 1
;; b 2
;; c [3 4 5]
;; d [1 2 3 4 5]

;; destructuring
(let [[_ _ x] [1 2 3]]
  x)

(let [[_ x _] [1 2 3]]
  x)

(let [[x _ y] [1 2 3]]
  [x y])



;; 4Clojure Question 83
;;
;; Write a function which takes a variable number of booleans.  Your function should return true if some of the parameters are true, but not all of the parameters are true.  Otherwise your function should return false.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [& rest]
  (if (every? (fn [x] x) rest)
    false
    (if (some (fn [x] x) rest)
      true
      false)))

;; identity do the 
(defn __ [& rest]
  (if (every? identity rest)
    false
    (if (some identity rest)
      true
      false)))

(defn __ [& rest]
  (let [len1 (count rest)
        len2 (count (filter (fn [x] x) rest))]
    len2))

;; if not all equal, true
(defn __ [& rest]
  (not= rest))

(= false (__ false false))
(= true (__ true false))
(= false (__ true))
(= true (__ false true false))
(= false (__ true true true))
(= true (__ true true true false))

(not-any? identity [true true true])
(not-any? identity [false true])
(not-any? identity [false false])
(every? identity [true true true])
(not-every? true? [true true true])



;; 4Clojure Question 61
;;
;; Write a function which takes a vector of keys and a vector of values and constructs a map from them.
;;
;; Restrictions (please don't use these function(s)): zipmap
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [list-a list-b]
  (zipmap list-a list-b))
(__ [:a :b :c] [1 2 3])

(defn __ [list-a list-b]
  (zipmap (interleave list-a list-b)))

(defn __ [list-a list-b]
  (partition-all 2 (interleave list-a list-b)))
(__ [:a :b :c] [1 2 3])

(defn __ [list-a list-b]
  (apply hash-map (interleave list-a list-b)))
(__ [:a :b :c] [1 2 3])

(defn __ [list-a list-b]
  (into {} (for [list-a a list-b b] [a b])))
(__ [:a :b :c] [1 2 3])

(defn __ [a b]
  (apply sorted-map (interleave a b)))
(__ [:a :b :c] [1 2 3])
(__ [:foo :bar] ["foo" "bar" "baz"])

(doc sorted-map)
;; -------------------------
;; clojure.core/sorted-map
;; ([& keyvals])
;;   keyval => key val
;;   Returns a new sorted map with supplied mappings.  If any keys are
;;   equal, they are handled as if by repeated uses of assoc.


(= (__ [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
(= (__ [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"})
(= (__ [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})



;; 4Clojure Question 166
;;
;; For any orderable data type it's possible to derive all of the basic comparison operations (&lt;, &le;, =, &ne;, &ge;, and &gt;) from a single operation (any operator but = or &ne; will work). Write a function that takes three arguments, a <var>less than</var> operator for the data and two items to compare. The function should return a keyword describing the relationship between the two items. The keywords for the relationship between <var>x</var> and <var>y</var> are as follows:
;;
;; 
;;
;; <ul>
;;
;; <li><var>x</var> = <var>y</var> &rarr; :eq</li>
;;
;; <li><var>x</var> &gt; <var>y</var> &rarr; :gt</li>
;;
;; <li><var>x</var> &lt; <var>y</var> &rarr; :lt</li>
;;
;; </ul>
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [fn1 x y]
  (cond (fn1 x y) :lt
        (fn1 y x) :gt
        :else     :eq))
(__ < 5 1)
(__ (fn [x y] (< (count x) (count y))) "pear" "plum")

(= :gt (__ < 5 1))
(= :eq (__ (fn [x y] (< (count x) (count y))) "pear" "plum"))
(= :lt (__ (fn [x y] (< (mod x 5) (mod y 5))) 21 3))
(= :gt (__ > 0 2))



;; 4Clojure Question 81
;;
;; Write a function which returns the intersection of two sets.  The intersection is the sub-set of items that each set has in common.
;;
;; Restrictions (please don't use these function(s)): intersection
;;
;; Use M-x 4clojure-check-answers when you're done!

(clojure.set/union #{0 1 2 3} #{2 3 4 5})

(require 'clojure.set)
;; require as
(require '[clojure.set :as cset])
(cset/union #{0 1 2 3} #{2 3 4 5})

(defn __ [a b]
  (let [c   (clojure.set/union a b)
        c-a (clojure.set/difference c a)
        c-b (clojure.set/difference c b)]
    (clojure.set/difference c (clojure.set/union c-a c-b))
    ))

(#{0 1 2 3} 4)
(contains? #{0 1 2 3} 4)

;; Collection works as a function. give each element and filter.
(defn __ [s1 s2]
  (set (filter s1 s2)))

(defn __ [s1 s2]
  (set (filter #(contains? s1 %) s2)))

(defn __ [a b]
  (-> (filter a b)
      set))

(= (__ #{0 1 2 3} #{2 3 4 5}) #{2 3})
(= (__ #{0 1 2} #{3 4 5}) #{})
(= (__ #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})


;; 4Clojure Question 66
;;
;; Given two integers, write a function which
;;
;; returns the greatest common divisor.
;;
;; Use M-x 4clojure-check-answers when you're done!

(filter (fn [x] (= 0 (rem 1023 x))) (range 1 (inc 1023)))

(defn __ [a b]
  (let [small (min a b)
        large (max a b)
        for-small (set (filter (fn [x] (= 0 (rem small x))) (range 1 (inc small))))
        for-large (set (filter (fn [x] (= 0 (rem large x))) (range 1 (inc large))))]

    (apply max (clojure.set/intersection for-small for-large))
    ))

(defn __ [a b]
  (apply max (filter
              #(= 0 (rem a %) (rem b %)) ; reminder 0 for both
              (range 1 (max a b)))))

(= (__ 2 4) 2)
(= (__ 10 5) 5)
(= (__ 5 7) 1)
(= (__ 1023 858) 33)




;; 4Clojure Question 62
;;
;; Given a side-effect free function f and an initial value x write a function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.
;;
;; Restrictions (please don't use these function(s)): iterate
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [f x]
  [x (f x) (f (f x))])

(defn __ [f x]
  (lazy-seq (loop [a x]
              a
              (recur (f a)))))


(defn positive-numbers
  ([] (positive-numbers 1))
  ([n] (cons n (lazy-seq (positive-numbers (inc n))))))
(take 5 (positive-numbers))

(defn __ [f x]
  (cons x (lazy-seq (__ f (f x)))))

(defn __ [f x] (reductions (fn [i _] (f i)) (repeat x)))
(take 5 (__ #(* 2 %) 1))

(defn __ [f x] (reduce (fn [i _] (f i)) (repeat x)))
;; (take 5 (__ #(* 2 %) 1))     ; this dies

(= (take 5 (__ #(* 2 %) 1)) [1 2 4 8 16])
(= (take 100 (__ inc 0)) (take 100 (range)))
(= (take 9 (__ #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))




;; 4Clojure Question 107
;;
;; <p>Lexical scope and first-class functions are two of the most basic building blocks of a functional language like Clojure. When you combine the two together, you get something very powerful called <strong>lexical closures</strong>. With these, you can exercise a great deal of control over the lifetime of your local bindings, saving their values for use later, long after the code you're running now has finished.</p>
;;
;; 
;;
;; <p>It can be hard to follow in the abstract, so let's build a simple closure. Given a positive integer <i>n</i>, return a function <code>(f x)</code> which computes <i>x<sup>n</sup></i>. Observe that the effect of this is to preserve the value of <i>n</i> for use outside the scope in which it is defined.</p>
;;
;; Use M-x 4clojure-check-answers when you're done!

(require 'clojure.contrib.math)

(defn __ [n]
  (fn [x]
    (apply * (repeat n x))))

(defn __ [n]
  (partial (Math/pow % n)))

((__ 4) 3)

(take 1 ((partial (- % 2)) 5))


(= 256 ((__ 2) 16),
       ((__ 8) 2))
(= [1 8 27 64] (map (__ 3) [1 2 3 4]))
(= [1 2 4 8 16] (map #((__ %) 2) [0 1 2 3 4]))



;; 4Clojure Question 99
;;
;; Write a function which multiplies two numbers and returns the result as a sequence of its digits.
;;
;; Use M-x 4clojure-check-answers when you're done!

(def __ [a b]
  ((* a b)))

(= (__ 1 1) [1])
(= (__ 99 9) [8 9 1])
(= (__ 999 99) [9 8 9 0 1])


