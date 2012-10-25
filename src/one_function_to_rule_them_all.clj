(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(declare my-interpose)

(defn str-cat [a-seq]
  (apply str (my-interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce #(concat %1 (list x) (list %2)) (list (first  a-seq)) (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [x y] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq)
   (reduce max a-seq)])

(defn insert [sorted-seq n]
  (concat (take-while #(> n %1) sorted-seq) [n] (drop-while #(> n %1) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x]
     (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x & more]
     (+ 1 (count more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & more] (reduce * x more)))

(defn pred-and
  ([] (fn [x] true))
  ([x] (fn [z] (x z)))
  ([x y] (fn [z] (and (x z) (y z))))
  ([x y & more]
     (reduce pred-and (pred-and x y) more)))

(defn my-map
  ([f x] (map f x))
  ([f x & more] (map #(apply f %)
                     (partition (+ 1 (count more)) 
                                (apply interleave (cons x more))))))
  