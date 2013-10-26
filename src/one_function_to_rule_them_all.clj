(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (let [f (fn [aseq string] (conj (conj aseq string) x))]
    (cond
      (empty? a-seq) '()
      (empty? (rest a-seq)) a-seq
      :else (reverse (rest (reverse (reduce f [] a-seq)))))))

(defn my-count [a-seq]
  (let [counter (fn [count x] (inc count))]
    (if (empty? a-seq)
      0
      (reduce counter 0 a-seq))))

(defn my-reverse [a-seq]
  [:-])

(defn min-max-element [a-seq]
  (let [f (fn [a-vec x] (let [[a b] a-vec]
                          (assoc
                            (assoc a-vec 0 (min a x)) 1 (max b x))))]
    (if (empty? a-seq)
      []
      (reduce f (vector (first a-seq) (first a-seq)) a-seq))))

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
