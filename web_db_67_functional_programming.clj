;;; Web and DB 67: Introduction to Functional Programming
;; The book examples were shown in Haskell.
(require 'clojure.test)

;; Multiply by index and sum
;;
;; signature: vector -> number
;; multiply each element of the vector with the index and add all up
;; (defn calc [xs]
;;   nil)
(defn calc [xs]
  (->> (interleave (range) xs)
       (partition-all 2          ,  )
       (map (fn [[a b]] (* a b)) ,  )
       (reduce +                 ,  )))

(clojure.test/is (= (calc [10 20 30 40 50]) 400))


;; Extract the max duplicated part of the string
;;
;; String -> String
;; Return the longest overlapping regions in the string given
;;
;; (defn max-dup-str [st]
;;   :str)

;; Obtain all possible suffixes
(defn suffixes [st]
  (let [st-seq (seq st)]
    (loop [acc []
           sseq st-seq]
      ;;
      (if (empty? sseq)
        (conj acc '())
        (recur (conj acc sseq) (rest sseq))))))
;;
(suffixes "mississippi")

;; Sort suffixes
(defn sorted-suffixes [st]
  (sort (map (fn [sseq] (clojure.string/join sseq)) (suffixes st))))
;;
(sorted-suffixes "mississippi")

;; Create pairs of neighboring suffixes
;; juxtaposing with the elements of the (rest xs) is the common way
(defn pairs [st]
  (let [sorted-suff (sorted-suffixes st)]
    (partition-all 2 (interleave sorted-suff (rest sorted-suff)))))
;;
(pairs "mississippi")

;; For a pair, calculate the length of match
;; loop macro solution
(defn match-length [[a b]]
  (loop [acc 0
         ssta (seq a)
         sstb (seq b)]
    (cond
     (or (empty? ssta) (empty? sstb)) acc
     (not= (first ssta) (first sstb)) acc
     :else (recur (inc acc) (rest ssta) (rest sstb)))))
;;
;; take-while solution (solution seen in the magazine)
(defn match-length [[a b]]
  (->> (interleave a b)
       (partition-all 2                      ,  )
       (take-while #(= (first %) (second %)) ,  )
       (count                                ,  )))
;;
(map match-length (pairs "mississippi"))

;; Finally, use all these parts to get the maximum duplication part of the string
(defn max-dup-str [st]
  (let [max-match-pair (last (sort-by  match-length (pairs st)))]
    (clojure.string/join (take (match-length max-match-pair) (first max-match-pair)))))
;;
(max-dup-str "mississippi")
;;
(clojure.test/is (= (max-dup-str "mississippi")
                    "issi"))
(clojure.test/is (= (max-dup-str "Ask not what your country can do for you, but what you can do for your country.")
                    " can do for you"))

