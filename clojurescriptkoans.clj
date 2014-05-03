;;; http://clojurescriptkoans.com/

;;; Run-time polymorphism
(defn hello
  ([] "Hello World!")
  ([a] (str "Hello, you silly " a "."))
  ([a & more] (str "Hello to this group: "
                   (apply str
                          (interpose ", " (concat (list a) more)))
                   "!")))
(hello)
(hello "world")
(hello "kazuki" "yoshida")
(def a '("kazuki" "tomoko" "shinya"))
a
(concat (list (first a)) (rest a))



(defmulti diet (fn [x] (:eater x)))
(defmethod diet :herbivore [a] )
(defmethod diet :carnivore [a] )
(defmethod diet :default [a])

(= "Bambi eats veggies." (diet {:species "deer", :name "Bambi", :age 1, :eater :herbivore}))

