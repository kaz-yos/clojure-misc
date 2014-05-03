;;; macro test

(defmacro unless [expr form]
  (list 'if expr        ; 'if not evaluated at macroexpansion time.
        nil
        form))

(unless false (println "this should print"))
(unless true (println "this should not print"))


(macroexpand-1 '(unless false (println "this should print")))

(macroexpand '(unless false (println "this should print")))

(macroexpand-1 '(when true (println "a") (println "b")))
;; (if true (do (println "a") (println "b")))

