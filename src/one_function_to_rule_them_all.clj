(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [w1 w2] (str w1 " " w2)) a-seq)))

(defn my-interpose [x a-seq]
  (let [reducer-fn (fn [current-accum-val next-item]
                        (if (empty? current-accum-val)
                        (conj current-accum-val next-item)
                        (conj (conj current-accum-val x) next-item)))]
  (reduce reducer-fn '[] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [current-accum-val _] (inc current-accum-val)) 0 a-seq))

(defn my-reverse [a-seq]
    (let [reducer-fn (fn [current-accum-val next-item]
                          (assoc current-accum-val 0 next-item))]
                          ; can shift by 1 for sizeof(a-seq)
    (reduce reducer-fn '[] a-seq)))

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
