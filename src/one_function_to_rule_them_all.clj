(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
  (reduce (fn [x z] (str x " " z)) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [y z] (conj y z x)) () (reverse a-seq))))

(defn my-count [a-seq]
  (let [add (fn [x _] (inc x))]
  (reduce add 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev (fn [x y] (cons y x))]
  (reduce rev () a-seq)))

(defn min-max-element [a-seq]
  [:-])

(defn insert [sorted-seq n]
  [:-])

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  [:-])

(defn minus [x]
  :-)

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
