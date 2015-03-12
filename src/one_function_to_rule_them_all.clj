(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce #(str % " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) ()
    (reduce (fn [m n] (conj m x n)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [acc e] (inc acc))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [acc e] (cons e acc))]
    (reduce reverser [] a-seq)))

(defn min-max-element [a-seq]
  (if (empty? a-seq) [nil nil]
    [(reduce min a-seq)(reduce max a-seq)]))

(defn insert [sorted-seq n]
  (let [my-split (split-with (fn [k] (> n k)) sorted-seq)]
    (concat (first my-split) (list n) (first (rest my-split)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [helper (fn [my-set e]
                 (if (contains? my-set e)
                   (disj my-set e)
                   (conj my-set e)))]
    (reduce helper #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [k] true))
  ([x] (fn [k] (x k)))
  ([x y] (fn [k] (and (x k) (y k))))
  ([x y & more]
   (reduce pred-and (pred-and x y) more)))

(defn my-map [f a-seq]
  [:-])
