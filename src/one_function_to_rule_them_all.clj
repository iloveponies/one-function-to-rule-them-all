(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
 (reduce concat [] a-seq))

(defn str-cat [a-seq]
  :-)

(defn my-interpose [x a-seq]
  [:-])

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

(defn minus 
   ([x](- x))
   ([x y] (- x y)))

(defn count-params
 ([& more] (count more)))

(defn my-* 
 ([] 1)
 ([x] x)
 ([x y] (* x y))
 ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and 
([] (fn [x] true))  
([z] (fn [x] (z x)))
([z y] (fn [x] (and (z x) (y x))))
([z y & more] (reduce (fn [z y] (fn [x] (and (z x) (y x))))
              (fn [x] (and (z x) (y x))) more)))

(defn my-map [f a-seq]
  [:-])
