(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [s t] (str s " " t)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [s t] (conj s x t)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [counter elem] (inc counter)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[minimum maximum] elem]
    [(min minimum elem) (max maximum elem)]) [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) [n]
    (< n (first sorted-seq)) (cons n sorted-seq)
    :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

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
