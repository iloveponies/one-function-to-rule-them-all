(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq)
  )

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [f (fn [a b]
              (str a " " b))]
    (reduce f (first a-seq) (rest a-seq)))
  ))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
      []
      (let [f (fn [acc b]
                (concat acc [x b]))]
      (concat [(first a-seq)] (reduce f [] (rest a-seq)))
    )))


(defn my-count [a-seq]
  (let [f (fn [a b ] (inc a ))]
  (reduce f 0 a-seq)))

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
