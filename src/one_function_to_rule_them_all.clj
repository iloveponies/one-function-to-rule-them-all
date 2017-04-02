(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (let [interpose-space (fn [x y] (apply str (concat x " " y)))]
    (if (empty? a-seq)
      ""
      (reduce interpose-space (first a-seq) (rest a-seq)))))

(defn my-interpose [x a-seq]
  (let [interpose-x (fn [y z] (conj (conj y x) z))]
    (if (empty? a-seq)
      []
      (reduce interpose-x [(first a-seq)] (rest a-seq)))))


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
