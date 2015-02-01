(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
   (apply str [(str (first a-seq) " ")
               (reduce (fn [acc, x] (str acc " " x)) (rest a-seq))])))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) '()
    (drop-last (reduce (fn [acc j] (conj (conj acc j) x)) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc j] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc i] (conj acc (conj acc i))) [] a-seq))

(defn min-max-element [a-seq] [:-])

(defn insert [sorted-seq n] [:-])

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
