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
  (let [f (fn [a b] (cons b a))]
    (reduce f [] a-seq)))

(defn min-max-element [a-seq]
  (let [f (fn [minmax x] (if (> (first minmax) x)
                           [x (second minmax)]
                           (if (< (second minmax) x)
                             [(first minmax) x]
                             minmax)
                           ))]
    (reduce f [(first a-seq) (first a-seq)] a-seq)
    ))


(defn insert [sorted-seq n]
  (let [pred (fn [a] (> n a))
        i (count (take-while pred sorted-seq))]
    (concat (take i sorted-seq) [n] (drop i sorted-seq))
  ))



(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

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
