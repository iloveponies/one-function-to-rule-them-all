(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (reduce str "" (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (let [f (fn [a-vec y] (conj a-vec y x))
        coll (reduce f [] a-seq)]
    (reverse (rest (reverse coll)))))

(defn my-count [a-seq]
  (let [counter (fn [count e] (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce (fn [a-seq x] (cons x a-seq)) '() a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (if (empty? sorted-seq) [n]
    (loop [head '() tail sorted-seq]
      (if (empty? tail)
        (concat head (cons n '()))
        (if (< n (first tail))
          (concat head (cons n tail))
          (recur (reverse (cons (first tail) (reverse head)))
                 (rest tail)))))))
    

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
  (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x & more]
    (+ 1 (count more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & more] (reduce * x more)))

(defn pred-and
  ([] (fn [a] true ))
  ([p] p)
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more]
    (reduce pred-and (pred-and p q) more)))

(defn my-map [f a-seq]
  [:-])
