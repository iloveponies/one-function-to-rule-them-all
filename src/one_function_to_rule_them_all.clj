(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
(reduce concat a-seq)
)

(defn str-cat [a-seq]
 (if (empty? a-seq) ""
   (reduce str "" (interpose " " a-seq))
  )
)


(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq))
)

(defn my-count [a-seq]
  (let [counter (fn [e _]
                  (inc e))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
	(reduce (fn [reversed restseq] (cons restseq reversed)) [] a-seq)
)

(defn min-max-element [a-seq]
  (reduce (fn [[min-val max-val] vertaa]
            [(min min-val vertaa) (max max-val vertaa)])
          [(first a-seq) (first a-seq)] a-seq )
)

(defn insert [sorted-seq n]
  (sort (cons n sorted-seq))
)

(defn insertion-sort [a-seq]
	(reduce insert [] a-seq)
)

(defn parity [a-seq]
  [:-])

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)                           ; one parameter
  ([x y] 2)                         ; two parameters
  ([x y & more] (+ 2 (count more))) ; more than two parameters
)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
