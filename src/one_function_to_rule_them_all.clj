(ns one-function-to-rule-them-all)


(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (not (empty? a-seq))
    (reduce (fn [x y] (str x " " y)) a-seq)
    ""))

(defn my-interpose [x a-seq]
  (if (not (empty? a-seq))
    (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq))
    ()))

(defn my-count [a-seq]
  (reduce (fn [amount elem] (inc amount)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [rev-seq e] (cons e rev-seq)) [] a-seq))

(defn min-max-element [a-seq]
  [:-])

(defn insert [sorted-seq n]
  [:-])

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  [:-])

(defn minus ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
