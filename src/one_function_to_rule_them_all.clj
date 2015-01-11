(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [y z] (conj y x z)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [x y] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [x y] (cons y x)) [(first a-seq)] (rest a-seq))))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    [0 0]
    (reduce (fn [[xMin xMax] y] [(min xMin y) (max xMax y)])
            [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (concat (take-while (fn [x] (< x n)) sorted-seq)
          (cons n (drop-while (fn [x] (< x n)) sorted-seq))))

(defn insertion-sort [a-seq]
  (if (empty? a-seq)
    '()
    (reduce insert [(first a-seq)] (rest a-seq))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y & more] (reduce my-* (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more] (reduce pred-and (pred-and p q) more)))

(defn my-map
  ([f seq1] (if (empty? seq1)
              nil
              (cons (f (first seq1)) (my-map f (rest seq1)))))
  ([f seq1 seq2] (if (or (empty? seq1) (empty? seq2))
                   nil
                   (cons (if (coll? (f (first seq1) (first seq2)))
                           (flatten (f (first seq1) (first seq2)))
                           (f (first seq1) (first seq2)))
                         (my-map f (rest seq1) (rest seq2)))))
  ([f seq1 seq2 & more] (reduce
                         (fn [a-seq b-seq] (my-map f a-seq b-seq))
                         (my-map f seq1 seq2)
                         more)))
