(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [ys y] (conj ys x y)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [a _] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [xs x] (cons x xs)) '() a-seq))

(defn min-max-element [xs]
  (reduce 
    (fn [[min_ max_] x] 
      [(min min_ x) (max max_ x)])
    [(first xs) (first xs)]
    (rest xs)))

(defn insert [sorted-seq n]
  (loop [head []
         tail sorted-seq]
    (if (and (not (empty? tail)) (< (first tail) n))
      (recur (conj head (first tail)) (rest tail))
      (concat head (cons n tail)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))
  
(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x & xs] (reduce * x xs)))

(defn pred-and 
  ([] (fn [t] true))
  ([x] (fn [t] x))
  ([x y] (fn [t] (and (x t) (y t))))
  ([x y & xs] (reduce pred-and (cons x (cons y xs)))))

(defn my-map [f a-seq]
  [:-])
