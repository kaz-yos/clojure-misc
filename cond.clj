;;; cond

(defn how-many [n]
  (cond
   (zero? n) "none"
   (= n 1) "one"
   (= n 2) "two"
   (> n 2) "many"))

(map how-many [0 1 2 3 4])


;; Emacs Lisp
;; (defun how-many (n)
;;   (cond
;;    ((zerop n) "none")
;;    ((= n 1) "one")
;;    ((= n 2) "two")
;;    ((> n 2) "many")))
;; (mapcar #'how-many '(0 1 2 3 4))
