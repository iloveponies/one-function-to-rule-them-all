(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (let [[y & ys] a-seq]
      (reduce #(concat %1 (list x %2)) (list y) ys))))

(defn my-count [a-seq]
  (reduce (fn [s _] (inc s)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (when-not (empty? a-seq)
    (let [[x & xs] a-seq]
      (reduce (fn [[mi ma] k] [(min k mi) (max k ma)]) [x x] xs))))

(defn insert [sorted-seq n]
  (let [[xs ys] (split-with #(< % n) sorted-seq)]
    (concat xs (cons n ys))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn- toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([_ & more] (inc (count more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
     (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [_] true))
  ([p] (fn [x] (p x)))
  ([p & ps] (fn [x] (reduce #(and %1 (%2 x)) (p x) ps))))

(defn my-map
  ([f xs]
     (if (empty? xs)
       '()
       (cons (f (first xs)) (my-map f (rest xs)))))
  ([f xs & more]
     (let [n (inc (count more))]
       (my-map #(apply f %) (partition n (apply interleave (cons xs more)))))))
