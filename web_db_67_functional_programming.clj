;;; Web and DB 67: Introduction to Functional Programming
;; The book examples were shown in Haskell.
(require 'clojure.test)

;; Chapter 2
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


;; Chapter 3
;; String -> String
;; Return the longest overlapping regions in the string given
;;
;; (defn max-dup-str [st]
;;   :str)
(defn max-dup-str [st]
  (mapcat str (reverse (reductions (fn [a b] (reverse (conj a b))) [] (reverse st)))))



(max-dup-str "mississippi")
;;
(clojure.test/is (= (max-dup-str "mississippi")
                    "issi"))
(clojure.test/is (= (max-dup-str "Ask not what your country can do for you, but what you can do for your country.")
                    "can do for you"))

