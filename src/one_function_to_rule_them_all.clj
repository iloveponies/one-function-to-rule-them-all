(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [str-helper (fn [x y] (str x " " y))]
    (reduce str-helper a-seq))))

(defn my-interpose [x a-seq]
  (let [insert-x (fn [a b] (conj a x b))]
    (rest (reduce insert-x [] a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [count elem]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverse-helper (fn [rev-seq elem]
                         (cons elem rev-seq))]
    (reduce reverse-helper [] a-seq)))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (loop [first-seq []
         last-seq sorted-seq]
    (if (and (not (empty? last-seq)) (< (first last-seq) n))
      (recur (conj first-seq (first last-seq)) (rest last-seq))
      (concat (conj first-seq n) last-seq))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [set-parity (fn [a-set elem]
                    (if (contains? a-set elem)
                      (disj  a-set elem)
                      (conj a-set elem)))]
  (reduce set-parity #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  [& more] (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred-a] (fn [x] (pred-a x)))
  ([pred-a pred-b] (fn [x] (and (pred-a x) (pred-b x))))
  ([pred-a pred-b & more] (reduce pred-and (pred-and pred-a pred-b) more)))

(defn split [seqs]
  (for [index (range 0 (count (first seqs)))]
    (reduce (fn [acc x] (conj acc (get x index))) [] seqs)))

(defn my-map [f & more]
  (loop [ret [] list (split more)]
    (if (empty? list)
    ret
    (recur (conj ret (apply  f (first list))) (rest list)))))
