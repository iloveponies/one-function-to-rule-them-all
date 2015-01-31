(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (apply str (reduce (fn [x y] (concat x " " y)) a-seq))))

(defn my-interpose [x a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (apply conj (reduce (fn [a b] (vector a x b)) a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [count x] (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [reverse-seq x]
                   (cons x reverse-seq))]
    (reduce reverser [] a-seq)))

(defn min-max-element [a-seq]
  (let [min-max (fn [elems x]
                  (cond
                    (empty? elems) (vector x x)
                    (< x (first elems)) (assoc elems 0 x)
                    (> x (second elems)) (assoc elems 1 x)
                    :else elems))]
    (reduce min-max [] a-seq)))


(defn insert [sorted-seq n]
  (let [split-seq (split-with (partial > n) sorted-seq)]
    (concat (first split-seq) (conj (second split-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set x]
                 (if (contains? a-set x)
                   (disj a-set x)
                   (conj a-set x)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x & more] (+ 1 (count more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))


(defn pred-and
  ([] (fn [x] (and true)))
  ([x] x)
  ([x y] (fn [n] (and (x n) (y n))))
  ([x y & more] (fn [n]
                  (reduce (fn [acc y] (and acc(y n))) (and (x n) (y n)) more))))

(defn my-map [f & more]
  (reduce f more))
