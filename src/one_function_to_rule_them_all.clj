(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
 (if (empty? a-seq)
   ""
   (reduce (fn [a b] (str a " " b)) a-seq )))
 
(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reverse (drop 1 (reduce (fn [a b] (conj a b x) ) '() a-seq))))
  )

(defn my-count [a-seq]
  (if (empty? a-seq)
    0
    (reduce (fn [a b] (inc a)) 0 a-seq)))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [a b] (conj a b)) '() a-seq)))

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