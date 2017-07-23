(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce
    concat
    ()
    a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce
      (fn [word1 word2] (str word1 " " word2))
      a-seq)))


(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce
      (fn [item1 item2]
        (conj item1 x item2))
      (conj '() (first (reverse a-seq)))
      (rest (reverse a-seq)))))

(defn my-count [a-seq]
  [:-])

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