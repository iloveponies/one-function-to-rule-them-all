(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [s1 s2] (str s1 " " s2)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce 
    (fn [accum e]
      (if (empty? accum)
        [e]
        (conj accum x e)))
    [] a-seq))

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