(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (cons (first a-seq) (reverse (reduce (fn [initial y] (conj (conj initial y) x)) [] (reverse (rest a-seq)))))))

(defn my-count [a-seq]
  (reduce (fn [initial x] (inc initial)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [initial x] (cons x initial)) '() a-seq))

(defn min-max-element [a-seq]
  [(reduce (fn [initial x] (if (< x initial) x initial)) a-seq) (reduce (fn [initial x] (if (> x initial) x initial)) a-seq)])

(defn insert [sorted-seq n]
  (loop [smaller []
         sorted-seq sorted-seq]
    (if (or (empty? sorted-seq) (>= (first sorted-seq) n))
      (concat smaller [n] sorted-seq)
      (recur (conj smaller (first sorted-seq)) (rest sorted-seq)))))

(defn insertion-sort [a-seq]
  (reduce (fn [sorted x] (insert sorted x)) [] a-seq))

(defn parity [a-seq]
  (set (reduce (fn [paritities x] (if (contains? (set paritities) x) (remove (fn [y] (= x y)) paritities) (conj paritities x))) #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred] (fn [x] (pred x)))
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more]
   (reduce (fn [initial p] (fn [x] (and (initial x) (p x)))) (pred-and pred1 pred2) more)))

(defn my-map [f a-seq]
  [:-])
