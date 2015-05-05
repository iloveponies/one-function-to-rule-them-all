(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (reduce str "" (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [helper (fn [newseq elem]
                   (if (empty? newseq)
                     (conj newseq elem)
                     (conj (conj newseq x) elem)))]
      (reduce helper [] a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [count x]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [helper (fn [new x]
                 (conj new x))]
    (reduce helper '() a-seq)))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    a-seq
    [(reduce min a-seq) (reduce max a-seq)]))

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq) [n]
   (< n (first sorted-seq)) (cons n sorted-seq)
   :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [helper (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce helper #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p p2] (fn [x] (and (p x) (p2 x))))
  ([p p2 & more] (reduce pred-and (pred-and p p2) more)))

(defn my-map [f a-seq]
  [:-])
