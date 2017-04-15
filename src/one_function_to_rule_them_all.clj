(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if(empty? a-seq) (str "") (reduce str(interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (reduce (fn [z y] (if (empty? z)(conj z y) (conj z x y))) [] a-seq ))

(defn my-count [a-seq]
  (if(empty? a-seq) 0 (reduce (fn [x y] (inc x)) a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
   (cons (apply min a-seq) (cons (apply max a-seq) '())))

(defn insert [sorted-seq n]
   (sort (conj sorted-seq n)))

(defn insertion-sort [a-seq]
  (reduce insert '()  a-seq))

(defn minus
  ([x] (- x)) ([x y] (- x y)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
    (reduce toggle #{} a-seq))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1) ([x] x) ([x y] (* x y)) ([x y & more] (reduce * y more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more] (reduce pred-and (pred-and p q) more)))

(defn my-map [f a-seq]
  [:-])
