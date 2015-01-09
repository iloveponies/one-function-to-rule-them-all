(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (let [cons-x-cons (fn [elem a-seq]
                      (if (empty? a-seq)
                        (cons elem a-seq)
                        (cons elem (cons x a-seq))))]
    (reduce cons-x-cons '() a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [acc e]
                  (inc acc))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  [:-])

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
  (let [cons-f (fn [elem e-seq]
                 (cons (f elem) e-seq))]
    (reduce cons-f '() a-seq)))
