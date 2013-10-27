(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) (str "") (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (reduce (fn [z y] (if (empty? z) (conj [] y) (conj z x y))) [] a-seq))

(defn my-count [a-seq]
  (reduce (fn [x y] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [x y] [ (apply min (conj x y)) (apply max (conj x y))]) [] a-seq))

(defn insert [sorted-seq n]
  (sort (conj sorted-seq n)))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([& more]
    (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce my-* (* x y) more)))

(defn pred-and
  ([] (fn [z] true))
  ([x] (fn [z] (x z)))
  ([x y] (fn [z] (and (x z) (y z))))
  ([x y & more]
    (reduce pred-and (pred-and x y) more)))

(defn my-map [f a-seq]
  )
