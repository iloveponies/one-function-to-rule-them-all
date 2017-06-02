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
    (reduce #(conj %1 x %2) (vector (first a-seq)) (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc x] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) [] a-seq))

(defn min-max-element [a-seq]
  (reduce
    (fn [min-max x] [(min (first min-max) x) (max (last min-max) x)])
    [(first a-seq) (first a-seq)]
    a-seq))

(defn insert [sorted-seq n]
  (loop [indx 0
         smaller '[]
         larger sorted-seq]
    (cond
      (empty? larger)
        (conj smaller n)
      (< n (first larger))
        (concat smaller (vector n) larger)
      :else
        (recur (inc indx) (conj smaller (first larger)) (rest larger)))))

(defn insertion-sort [a-seq]
  (reduce insert '[] a-seq))

(defn parity [a-seq]
  (let [helper (fn [sq elem] (if (contains? sq elem)
                               (disj sq elem)
                               (conj sq elem)))]
    (reduce helper #{} a-seq)))

(defn minus
  ([x] (* x -1))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x & more] (reduce * x more)))

(defn pred-and
  ([] (fn [x] true))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & preds] (fn [x]
                     (reduce (fn [a b] (and a (b x))) (and (p1 x) (p2 x)) preds))))

(defn my-map
  ([f a-seq] (reduce #(conj %1 (f %2)) [] a-seq))
  ([f a-seq & seqs] true))
