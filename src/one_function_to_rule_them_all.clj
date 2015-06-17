(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [prev curr] (str prev " " curr)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [prev curr] (conj prev x curr)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [prev curr] (inc prev)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [prev curr] (conj prev curr)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [prev curr]
            (let [[min-prev max-prev] prev]
              (vector (min min-prev curr) (max max-prev curr)))) [(first a-seq) (first a-seq)] (rest a-seq)))

(defn insert [sorted-seq n]
  (sort (conj sorted-seq n)))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce (fn [prev curr]
            (if (contains? prev curr)
              (disj prev curr)
              (conj prev curr))) #{} a-seq))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & args] (reduce * (* x y) args)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & args] (reduce (fn [prev curr] (pred-and prev curr)) (pred-and p1 p2) args)))

(defn my-map [f a-seq]
  [:-])