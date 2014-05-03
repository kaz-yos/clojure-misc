;;; 2014-04-12 ->> -> macros

;; http://clojuredocs.org/clojure_core/clojure.core/-%3E%3E#example_191
(->> (range)            ; 1. Create an infinite sequence of 0, 1, 2, 3, ...
     (map #(* % %))     ; 2. Square each element
     (filter even?)     ; 3. Filter only even numbers
     (take 10)          ; 4. Take first 10 elements from the infinite sequence
     (reduce +))        ; 5. Reduce by adding them up

