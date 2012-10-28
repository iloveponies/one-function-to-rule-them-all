(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [word-1 word-2] (str word-1 " " word-2)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce 
     (fn [item-1 item-2]
       (conj (if (vector? item-1) item-1 (vector item-1)) x item-2))
     a-seq)))

(defn my-count [a-seq]
  (if (empty? a-seq)
    0
    (reduce (fn [len a-seq] (inc len)) 0 a-seq)))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [elems elem] (cons elem elems)) () a-seq)))

(defn min-max-element [a-seq]
  (let [helper (fn [[cur-min cur-max] elem]
                 (str "(" cur-min "," cur-max ")"))]
;                 (if (empty? (vector cur-min))
;                   [elem elem]
;                   (vector (if (< elem cur-min) elem cur-min)
;                    (if (> elem cur-max) elem cur-max))))]
    (if (empty? a-seq)
      nil
      (reduce helper [] a-seq))))

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