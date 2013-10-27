(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [s st] (str s " " st)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [s st] (conj (conj s x) st)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [c _] (inc c)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [res x] (cons x res)) () a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    ()
    (reduce (fn [[mi ma] x] [(min mi x) (max ma x)]) [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (loop [res () s sorted-seq]
    (if (or (empty? s) (< (first s) n))
         (reduce conj res (cons s n))
      (recur (conj res (first s)) (rest s)))))

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
