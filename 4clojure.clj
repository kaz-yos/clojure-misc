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

(defn __ [a b]
  (map read-string (clojure.string/split (str (* a b)) #"")))

(defn __ [a b]
  (clojure.string/split (str (* a b)) #""))

(defn __ [a b]
  (map #(. Integer parseInt %) (clojure.string/split (str (* a b)) #"")))

(defn __ [a b]
  (map #(Integer/parseInt %) (clojure.string/split (str (* a b)) #"")))

;; create number as a string, map the letters, covert each letter to string and back to interger.
(fn __ [a b]
  (map #(Integer/parseInt (str %))  (str (* a b))))



;; use read-string to evaluate "1"
(defn __ [a b]
  (vec (map read-string (clojure.string/split (str (* a b)) #""))))

(= (__ 1 1) [1])
(= (__ 99 9) [8 9 1])
(= (__ 999 99) [9 8 9 0 1])


;; http://stackoverflow.com/questions/2640169/whats-the-easiest-way-to-parse-numbers-in-clojure
(let [input "5  10  0.002\n4  12  0.003"]
        (re-seq #"[\d.]+" input))
(let [input "5  10  0.002\n4  12  0.003"]
        (map read-string (re-seq #"[\d.]+" input)))


;; 4Clojure Question 90
;;
;; Write a function which calculates the <a href="http://en.wikipedia.org/wiki/Cartesian_product"> Cartesian product</a> of two sets.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [a b]
  (set (mapcat (fn [x]
                 (map (fn [y] [x y])
                      b))
               a)))

(defn __ [x1 x2]
  (set (for [i1 x1 i2 x2]       ; this does all possible combinations
         [i1 i2])))


(= (__ #{"ace" "king" "queen"} #{"&#9824;" "&#9829;" "&#9830;" "&#9827;"})
   #{["ace"   "&#9824;"] ["ace"   "&#9829;"] ["ace"   "&#9830;"] ["ace"   "&#9827;"]
     ["king"  "&#9824;"] ["king"  "&#9829;"] ["king"  "&#9830;"] ["king"  "&#9827;"]
     ["queen" "&#9824;"] ["queen" "&#9829;"] ["queen" "&#9830;"] ["queen" "&#9827;"]})

(= (__ #{1 2 3} #{4 5})
   #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})

(= 300 (count (__ (into #{} (range 10))
                  (into #{} (range 30)))))



;; 4Clojure Question 63
;;
;; Given a function f and a sequence s, write a function which returns a map.  The keys should be the values of f applied to each item in s.  The value at each key should be a vector of corresponding items in the order they appear in s.
;;
;; Restrictions (please don't use these function(s)): group-by
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [f s]
  (group-by f s))

(defn __ [f s]
  (let [ks (map f s)
        vs s]

    (interleave ks vs)))

(__ #(> % 5) [1 3 6 8])


(defn __ [f s]
  (let [ks (map f s)
        vs s]

    (partition 2 (interleave ks vs))))

(__ #(> % 5) [1 3 6 8])


(defn __ [f s]
  (let [ks (map f s)]

    (map (fn [x] (zipmap [(first x)] [(last x)])) (partition 2 (interleave ks s)))))

(__ #(> % 5) [1 3 6 8])
(__ #(apply / %) [[1 2] [2 4] [4 6] [3 6]])


(apply merge-with vector '({false 1} {false 3} {true 6} {true 8}))
(apply merge-with list '({1/2 [1 2]} {1/2 [2 4]} {2/3 [4 6]} {1/2 [3 6]}))

(vals (apply merge-with list '({1/2 [1 2]} {1/2 [2 4]} {2/3 [4 6]} {1/2 [3 6]})))

'({1/2 [1 2]} {1/2 [2 4]} {2/3 [4 6]} {1/2 [3 6]})

(partition-by (fn [x] (keys x)) '({1/2 [1 2]} {1/2 [2 4]} {2/3 [4 6]} {1/2 [3 6]}))

(into [] '(1 2 3 4))

(defn __ [f s]
  (let [ks (map f s)]

    (apply merge-with vector
           (map (fn [x] (zipmap [(first x)] [(last x)])) (partition 2 (interleave ks s))))))

(defn __ [f s]
  (let [ks (map f s)]

    (apply merge-with (fn [x y] (conj [x] y))
           (map (fn [x] (zipmap [(first x)] [(last x)])) (partition 2 (interleave ks s))))))

(__ #(> % 5) [1 3 6 8])
(__ #(apply / %) [[1 2] [2 4] [4 6] [3 6]])

(map (fn [x] {(key x) (val x)}) {2/3 [4 6], 1/2 [[[1 2] [2 4]] [3 6]]})

(map (fn [x] {(key x)
              (val x)})
     {2/3 [4 6], 1/2 [[[1 2] [2 4]] [3 6]]})


(= (__ #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})
(= (__ #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
   {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})
(= (__ count [[1] [1 2] [3] [1 2 3] [2 3]])
   {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})




;; 4Clojure Question 122
;;
;; Convert a binary number, provided in the form of a string, to its numerical value.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [s]
  (let [a (map #(if (= "1" %) 1 0) (clojure.string/split s #""))
        b (iterate (fn [x] (* 2 x)) 1)]

    (->> (interleave a b                )
         (partition-all 2               )
         (map (fn [x] (apply * x))      )
         (apply +)
         )
    )
  )

(__ "1001")
(map (fn [x] (apply * x)) '((1 1) (0 2) (0 4) (1 8)))
(apply + (map (fn [x] (apply * x)) '((1 1) (0 2) (0 4) (1 8))))


;; clean
(defn __ [s]
  (let [a (map #(if (= "1" %) 1 0) (clojure.string/split s #""))
        a (reverse a)
        b (iterate (fn [x] (* 2 x)) 1)]

    (->> (interleave a b                )
         (partition-all 2               )
         (map (fn [x] (apply * x))      )
         (apply +                       ))))

;; Integer/parseInt is good
(Integer/parseInt "110" 2)

(= 0     (__ "0"))
(= 7     (__ "111"))
(= 8     (__ "1000"))
(= 9     (__ "1001"))
(= 255   (__ "11111111"))
(= 1365  (__ "10101010101"))
(= 65535 (__ "1111111111111111"))




;; 4Clojure Question 88
;;
;; Write a function which returns the symmetric difference of two sets.  The symmetric difference is the set of items belonging to one but not both of the two sets.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [a b]
  (let [intrsct (clojure.set/intersection a b)]

    (clojure.set/union (clojure.set/difference a intrsct)
                       (clojure.set/difference b intrsct))
    ))

(set (concat #{1 2 3 4 5 6} #{1 3 5 7}))

;; clojure.core/disj
;; ([set] [set key] [set key & ks])
;;   disj[oin]. Returns a new set of the same (hashed/sorted) type, that
;;   does not contain key(s).
(defn __ [a b]
  (clojure.set/union (apply disj a b) (apply disj b a)))
;; (apply disj a b)     ; b is opened up

(defn __ [a b]
  (clojure.set/difference (set (concat a b)) (clojure.set/intersection a b)))

(= (__ #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
(= (__ #{:a :b :c} #{}) #{:a :b :c})
(= (__ #{} #{4 5 6}) #{4 5 6})
(= (__ #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})




;; 4Clojure Question 143
;;
;; Create a function that computes the <a href="http://en.wikipedia.org/wiki/Dot_product#Definition">dot product</a> of two sequences. You may assume that the vectors will have the same length.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [x y]
  (apply + (map #(apply * %) (partition-all 2 (interleave x y)))))

;; map can take multiple collections!!
(map * [1 2 3] [4 5 6])

(defn __ [x y]
  (apply + (map * x y)))

(= 0 (__ [0 1 0] [1 0 0]))
(= 3 (__ [1 1 1] [1 1 1]))
(= 32 (__ [1 2 3] [4 5 6]))
(= 256 (__ [2 5 6] [100 10 1]))




;; 4Clojure Question 120
;;
;; Write a function which takes a collection of integers as an argument.  Return the count of how many elements are smaller than the sum of their squared component digits.  For example: 10 is larger than 1 squared plus 0 squared; whereas 15 is smaller than 1 squared plus 5 squared.
;;
;; Use M-x 4clojure-check-answers when you're done!

(map read-string (map clojure.string/split (map str (range 30)) #""))

(->> (range 30)
     (map (fn [elt]
            (map #(* % %) (map read-string (clojure.string/split (str elt) #"")))
            )))


(->> (range 30)
     (map (fn [elt]
            (apply +
             (map #(* % %) (map read-string (clojure.string/split (str elt) #""))))

            )))

(->> (range 30)
     (map
      (fn [elt]
        (->> (clojure.string/split (str elt) #"")
             (map read-string )
             (map #(* % %)    )
             (apply +         ))
        )
      ))




(defn __ [s]
  (count (filter
          (fn [elt]
            (->> (clojure.string/split (str elt) #"")
                 (map   read-string   ) ; read-string does not work in 4clojure?
                 (map   #(* % %)      )
                 (apply +             )
                 (<     elt           )
                 )
            )
          s)
         )
  )

(= 8 (__ (range 10)))
(= 19 (__ (range 30)))
(= 50 (__ (range 100)))
(= 50 (__ (range 1000)))



;; 4Clojure Question 100
;;
;; Write a function which calculates the <a href="http://en.wikipedia.org/wiki/Least_common_multiple">least common multiple</a>.  Your function should accept a variable number of positive integers or ratios.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [a b]
  (let [infseq (iterate inc 1)
        rem-a (rem infseq a)
        rem-b (rem infseq b)]

    (filter (fn [x] (and (= 0 rem-a)
                         (= 0 rem-b)))
            infseq)
    ))


(defn __ [a b]
  (let [infseq (iterate inc 1)]

    (filter (fn [x] (and (integer? (/ x a))
                         (integer? (/ x b))))
            infseq)
    ))
(__ 2 3)

(defn __ [& rest]
  (let [infseq (iterate inc 1)]

    (filter (fn [x] (every?
                     #(integer? (/ x %))
                     rest
                     ))
            infseq)
    ))
(__ 2 3)
(__ 5 3 7)

(defn __ [& rest]
  (let [inf-s (iterate inc 1)]

    (first (filter (fn [x] (every?
                            #(integer? (/ x %))
                            rest
                            ))
                   inf-s))
    ))
(__ 2 3)
(__ 5 3 7)
(__ 3/4 1/6)


(defn __ [& rest]
  (let [inf-s (iterate inc 1)
        ]

    (first (filter (fn [x] (every?
                            #(= 0 (rem x %))
                            rest
                            ))
                   inf-s))
    ))

;; this is not looking for 3/2
(filter (fn [x] (every?
                 #(= 0 (rem x %))
                 [3/4 1/6]
                 ))
        (iterate inc 1))
(rem 3/2 3/4)
(denominator 3/2)
(denominator 5)

(__ 2 3)
(__ 5 3 7)
(__ 3/4 1/6)

(== (__ 2 3) 6)
(== (__ 5 3 7) 105)
(== (__ 1/3 2/5) 2)
(== (__ 3/4 1/6) 3/2)
(== (__ 7 5/7 2 3/5) 210)




;; 4Clojure Question 44
;;
;; Write a function which can rotate a sequence in either direction.
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [n s]
  (for [n (range n)]
    (conj (last s) (rest s))
    ))

(defn __ [n sq]
  (loop [s sq
         i 1]
    (if (> i n)
      s
      (recur (cons (last s) (butlast s)) (inc i)))
    ))

(defn __ [n sq]
  (loop [s sq
         i 1]
    (if (> i n)
      s
      (recur (conj (vec (rest s)) (first s)) (inc i)))
    ))
(__ 2 [1 2 3 4 5])
(__ 6 [1 2 3 4 5])

(defn __ [n sq]
  (loop [s sq
         i 0]
    (if (= i n)
      s
      (recur (conj (vec (rest s)) (first s)) (inc i)))
    ))
(__ 2 [1 2 3 4 5])
(__ 6 [1 2 3 4 5])

(defn __ [n sq]
  (loop [s sq
         i 0]
    (cond 
     (n > 0) (if (= i n)
               s
               (recur (conj (vec (rest s)) (first s)) (inc i)))

     (n < 0) (if (= i n)
               s
               (recur (cons (last s) (butlast s)) (dec i)))
     
     :else s)
    ))


(defn __ [n sq]
  (loop [s (if (>= n 0) sq (reverse sq))
         i 0]

    (if (= i (if (>= n 0) n (* -1 n)))

      ;; return result
      (if (>= n 0) s (reverse s))

      ;; recur
      (recur (conj (vec (rest s)) (first s)) (inc i)))
    ))

;;
(mod 2 5)
(mod -2 5)
(defn __ [n sq] (let [n (mod n (count sq))]
                  (concat (drop n sq)
                          (take n sq)
                          )))

(__ 2 [1 2 3 4 5])      ; take 2
(__ -2 [1 2 3 4 5])     ; take 3
(__ 6 [1 2 3 4 5])      ; take 1

(= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
(= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
(= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))
(= (__ 1 '(:a :b :c)) '(:b :c :a))
(= (__ -4 '(:a :b :c)) '(:c :a :b))




;; 4Clojure Question 173
;;
;; Sequential destructuring allows you to bind symbols to parts of sequential things (vectors, lists, seqs, etc.): <a href="http://clojure.org/special_forms#Special Forms--(let [bindings* ] exprs*)">(let [bindings* ] exprs*)</a>
;;
;; 
;;
;; Complete the bindings so all let-parts evaluate to 3.
;;
;; Use M-x 4clojure-check-answers when you're done!

(= 3
   (let [[f s] [+ (range 3)]]
     (apply f s))
   
   (let [[[f s] b] [[+ 1] 2]]
     (f s b))
   
   (let [[f s] [inc 2]]
     (f s)))



;; 4Clojure Question 50
;;
;; Write a function which takes a sequence consisting of items with different types and splits them up into a set of homogeneous sub-sequences. The internal order of each sub-sequence should be maintained, but the sub-sequences themselves can be returned in any order (this is why 'set' is used in the test cases).
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [s]
  (group-by class s))
(set (__ [1 :a 2 :b 3 :c]))

(defn __ [s]
  (group-by type s))
(set (__ [1 :a 2 :b 3 :c]))

(defn __ [s]
  (vals (group-by class s)))

(= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
(= (set (__ [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
(= (set (__ [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})



;; 4Clojure Question 54
;;
;; Write a function which returns a sequence of lists of x items each.  Lists of less than x items should not be returned.
;;
;; Restrictions (please don't use these function(s)): partition, partition-all
;;
;; Use M-x 4clojure-check-answers when you're done!

(defn __ [n coll]
  (if (< (count coll) n)
      []
      (cons (take n coll) (__ n (drop n coll)))) )


(= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
(= (__ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
(= (__ 3 (range 8)) '((0 1 2) (3 4 5)))




;; 4Clojure Question 43
;;
;; Write a function which reverses the interleave process into x number of subsequences.
;;
;; Use M-x 4clojure-check-answers when you're done!

(mapcat identity (repeat (range 3)))

(defn __ [coll n]
  (interleave (mapcat identity (repeat (range n))) coll)
  )
(__ [1 2 3 4 5 6] 2)
'(0 1 1 2 0 3 1 4 0 5 1 6)

(defn __ [coll n]
  (group-by first (partition-all 2 (interleave (mapcat identity (repeat (range n))) coll)))
  )
(__ [1 2 3 4 5 6] 2)
'{0 [(0 1) (0 3) (0 5)], 1 [(1 2) (1 4) (1 6)]}

(defn __ [coll n]
  (vals (group-by first (partition-all 2 (interleave (mapcat identity (repeat (range n))) coll))))
  )
(__ [1 2 3 4 5 6] 2)


(defn __ [coll n]
  (->> (range n)                ; thread this through the last argument
       (repeat                )
       (mapcat identity       )
       (#(interleave % coll)  ) ; thread through the first argument for this function
       (partition-all 2       )
       (group-by first        )
       (vals                  )
       (map #(map second %)   )
       )
  )
(__ [1 2 3 4 5 6] 2)


(map identity '([(0 1) (0 3) (0 5)] [(1 2) (1 4) (1 6)]))
(map (fn [v] (map second v)) '([(0 1) (0 3) (0 5)] [(1 2) (1 4) (1 6)]))


(defn __ [coll n]
  (map #(mapcat second %) (vals (group-by first (partition-all 2 (interleave (mapcat identity (repeat (range n))) coll)))))
  )

(= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
(= (__ (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
(= (__ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))
