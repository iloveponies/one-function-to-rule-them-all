(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (let [concat-with-spaces (fn [s1 s2] (str s1 " " s2))]
    (if (empty? a-seq)
      ""
      (reduce concat-with-spaces a-seq))))

(defn my-interpose [x a-seq]
  (let [concat-with-interpose (fn [s1 s2]
                                (conj s1 x s2))]
    (rest (reduce concat-with-interpose [] a-seq))))

(defn my-count [a-seq]
  (let [count-elements (fn [counter elem]
                         (inc counter))]
    (reduce count-elements 0 a-seq)))

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
