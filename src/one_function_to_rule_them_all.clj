(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc elem] (str acc " " elem)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [acc elem] (conj acc x elem)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [s _](inc s)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %) '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq) []
  (let [f (first a-seq)]
    (reduce #(identity [(min (% 0) %2) (max (% 1) %2)]) [f f] (rest a-seq)))))

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
