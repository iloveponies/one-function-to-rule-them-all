(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (concat (list) (reduce concat a-seq)))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
      (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  [:-])

(defn my-count [a-seq]
  (let [counter (fn [count e]
                    (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  [:-])

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  [:-])

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  [:-])

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& params] (count params)))

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
