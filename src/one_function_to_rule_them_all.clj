(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
(reduce concat () a-seq)
  )

(defn str-cat [a-seq]
   (if (empty? a-seq) ""
    (reduce (fn [x y] (str x" " y)) a-seq)
  )
)


(defn my-interpose [x a-seq]
  [:-])


(defn my-count [a-seq]
  (let [count (fn [cou x] (inc cou))]
    (reduce count 0 a-seq))
  )

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

(defn minus
 ([x] (- x) )
  ([x y] (- x y) )


)

(defn count-params [x]
  :-)

(defn my-*
 ([] 1)
  ([x] x)
  ([x y & abc] (reduce my-*(* x y ) abc))
)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
