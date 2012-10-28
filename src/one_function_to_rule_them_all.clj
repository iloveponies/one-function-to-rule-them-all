(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  [:-])

(defn my-interpose [x a-seq]
  [:-])

(defn my-count [a-seq]
  (let [counter (fn [cnt elems]
                  (inc cnt))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  [:-])

(defn insert [sorted-seq n]
  [:-])

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  [:-])

(defn minus 
  ([x]
    (- x))
  ([x y]
    (- x y)))

(defn count-params [& more]
  (my-count more))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce * (* x y) more)))

(defn pred-and 
  ([] 
    (fn [x] (zero? 0)))
  ([x] 
    (fn [param] (x param)))
  ([x y] 
    (fn [param] (and (x param) (y param))))
  ([x y & more] 
    (:-));(fn [param] ((reduce and (and x y) more) param)))
  )

(defn my-map [f a-seq]
  [:-])