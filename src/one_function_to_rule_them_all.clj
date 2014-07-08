(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

; There must be a more elegant approach.
(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [acc elt]
              (if (empty? acc)
                (list elt)
                (concat acc (list x elt)))) '() a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc x] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc elt] (cons elt acc)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [acc elt] (if (empty? acc)
                          [elt elt]
                          (let [[cur-min cur-max] acc]
                            [(min cur-min elt) (max cur-max elt)]))) [] a-seq))

; When in doubt, use brute force
(defn insert [sorted-seq n]
  (let [lower (filter (fn [x] (< x n)) sorted-seq)
        higher (filter (fn [x] (> x n)) sorted-seq)]
    (concat lower (list n) higher)))

(defn insertion-sort [a-seq]
  (reduce (fn [acc n] (insert acc n)) '() a-seq))

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