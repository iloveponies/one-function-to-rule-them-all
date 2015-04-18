(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a s] (str a " " s)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [a e] (conj a x e)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [a _] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a e] (cons e a)) [] a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [a e] [(min (get a 0) e) (max (get a 1) e)]) [100000 -100000] a-seq))

(defn insert [sorted-seq n]
  (loop [v []
         a-seq sorted-seq]
    (cond (empty? a-seq) (conj v n)
          (<= n (first a-seq)) (concat v (cons n a-seq))
          :else (recur (conj v (first a-seq)) (rest a-seq)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more] (count more))

(defn my-* [& more] (reduce * 1 more))

(defn pred-and
  ([& more] (fn [x] (loop [truth true
                           predicates more]
                     (cond (empty? predicates) truth
                           ((first predicates) x) (recur true (rest predicates))
                           :else false)))))

(defn my-map
  ([f & more] (if (= 1 (count more)) (reduce (fn [a x] (conj a (f x))) [] (first more))
   (let [c (count (first more))]
                (loop [i 0
                       acc []]
                  (if (= i c) acc
                    (let [elems (reduce (fn [a a-seq] (conj a (get a-seq i))) [] more)]
                      (recur (inc i) (conj acc (apply f elems))))))))))
