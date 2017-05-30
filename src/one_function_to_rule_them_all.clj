(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (> 2 (count a-seq))
    (sequence a-seq)
    (drop-last (reduce (fn [a b] (conj a b x)) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [x y] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [x y] (conj x y)) () a-seq))

(defn min-max-element [a-seq]
  (reduce
    (fn [x y]
      (if (< y (first x))
        (assoc x 0 y)
        (if (> y (nth x 1))
          (assoc x 1 y)
          x
          )
      ))
    [(first a-seq) (first a-seq)]
    a-seq))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (conj () n)
    (concat
      (filter (fn [x] (<= x n)) sorted-seq)
      (conj () n)
      (filter (fn [x] (> x n)) sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (* x -1))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (* x y (reduce * 1 more))))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
   (fn [x] (reduce (fn [a b]
             (and a (b x)))
             (and (p1 x) (p2 x))
            more))))


(defn my-map [f a-seq]
  [:-])
