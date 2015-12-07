(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq)
            )))

(defn my-interpose [x a-seq]
  (rest (apply concat (map (fn [elem] (conj (cons elem nil) x)) a-seq))))

(defn my-count [a-seq]
   (let [counter (fn [count _]
                   (inc count))]
     (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [acc elem]
                   (cons elem acc))]
    (reduce reverser '() a-seq)))

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
