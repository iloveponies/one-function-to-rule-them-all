(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
   (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (let [helper (fn [acc e]
                (if (empty? acc)
                 (conj acc e)
                 (conj acc x e)))]
    (reduce helper [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc b] (inc acc)) 0 a-seq))

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
