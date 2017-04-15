(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) "" (reduce (fn [e s] (str e " " s)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) a-seq (reduce (fn [e s] (conj e x s)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (concat [b] a)) [] a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (loop [c [] l sorted-seq]
    (cond
     (empty? l) (conj c n)
     (< (first l) n) (recur (conj c (first l)) (rest l))
     :else (concat c [n] l))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x)) ([x y] (- x y)))

(defn count-params ([] 0) ([x] 1) ([x y] 2) ([x y & more] (reduce (fn [a b] (inc a)) 2 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred1] (fn [x] (pred1 x)))
  ([pred1 & more]
   (fn [x] (reduce (fn [o p] (and o (p x))) (pred1 x) more))))

(defn my-map [f a-seq]
  [:-])
