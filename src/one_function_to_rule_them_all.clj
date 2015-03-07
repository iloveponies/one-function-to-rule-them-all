(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
  (concat (reduce (fn [a b] (conj (conj a x) b)) [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [count x] (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [f (fn [b-seq x] (cons x b-seq))]
    (reduce f '() a-seq)))

(defn min-max-element [a-seq]
  (let [f (fn [[min-val max-val] x] [(min min-val x) (max max-val x)])]
    (reduce f [(first a-seq) (first a-seq)] a-seq)))

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
