(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (or (nil? a-seq) (empty? a-seq))
    ""
    (reduce (fn [acc sek] (if (= "" acc) (str sek) (str acc " " sek))) "" a-seq)))

(defn my-interpose [x a-seq]
  (if (or (nil? a-seq) (empty? a-seq))
    '()
    (reduce (fn [acc sek] (if (empty? acc) (conj [] sek) (concat acc x sek))) [] a-seq)))

(defn my-count [a-seq]
  :-)

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
  [:-])
