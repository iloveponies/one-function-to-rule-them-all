(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if
    (empty? a-seq)
    ""
    (reduce #(str % " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if
    (empty? a-seq)
    []
    (rest (reduce #(conj % x %2) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc itm] (cons itm acc)) [] a-seq))

(defn min-max-element [a-seq]
  (if
    (empty? a-seq)
    []
    (reduce
      (fn [acc itm]
        (let [new-min (min itm (first acc))
              new-max (max itm (first (rest acc)))]
          [new-min new-max]))
      [(first a-seq) (first a-seq)]
      a-seq)))

(defn insert [sorted-seq n]
  (concat
    (take-while #(<= % n) sorted-seq)
    [n]
    (drop-while #(<= % n) sorted-seq)))

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
