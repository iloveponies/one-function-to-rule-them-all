(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(apply str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (let [coll (seq a-seq)]
    (cond
     (empty? coll) '()
     (empty? (next coll)) coll
     :else
     (seq (reduce #(conj %1 x %2) (vector (first coll)) (next coll))))))

(defn my-count [a-seq]
    (reduce (fn (inc %1)) 0 a-seq))

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
