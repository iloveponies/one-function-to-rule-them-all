(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
   (rest (reduce (fn [acc f] (conj acc x f)) []  a-seq)))

(defn my-count [a-seq]
  (reduce + (map (constantly 1) a-seq)))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq)
   (reduce max a-seq)])

(defn insert [sorted-seq n]
  (loop [acc []
         s sorted-seq
         nn n]
    (if (or (empty? s) (< nn (first s) ))
      (concat acc [nn] s)
      (recur (concat acc [(first s)]) (rest s) nn)
    )))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if(contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    ))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more))
 )


(defn my-*
  ([] 1)
  ([x] x)
  ([x & more]
    (reduce * x more)))

(defn pred-and
 ([] (fn[i]true))
 ([x] x)
 ([x y] (fn[i](and (x i) (y i))))
 ([x y & more]
    (fn[i] (and (x i) (y i) (reduce (fn[acc x](and acc x)) (map (fn[f](f i)) more) )))))

(defn my-map [f & a-seq]
   (if (<=(count  a-seq) 1)
     (map f (first a-seq))
     (map (fn[x](apply f x))(partition (count a-seq) (apply interleave a-seq))
  )))
