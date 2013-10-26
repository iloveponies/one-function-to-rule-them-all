(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (clojure.string/join " " a-seq))

(defn my-interpose [x a-seq]
  (drop 1 (interleave (repeat x) a-seq)))

(defn my-count [a-seq]
  (reduce (fn [c x] (inc c)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  [(apply min a-seq) (apply max a-seq)])

(defn insert [sorted-seq n]
  (sort (conj sorted-seq n)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x]   (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([]         1)
  ([x & more] (reduce * x more)))

(defn pred-and
  ([]           (fn [a] true))
  ([x]          (fn [a] x a))
  ([x y]        (fn [a] (and (x a) (y a))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn my-map [f & a-seq]
  (if (not-any? empty? a-seq)
    (conj
      (apply (partial my-map f) (map rest a-seq))
      (apply f (map first a-seq)))))
