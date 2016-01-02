(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn[x p] (str x " " p)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn[h p] (conj (conj h x) p)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn[x p] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn[x p] (cons p x)) [] a-seq))

(defn min-max-element [a-seq]
  (reduce
    (fn[x p] (let [[f s] x] (assoc (assoc x 0 (min f p)) 1 (max s p))))
    [(first a-seq) (first a-seq)]
    (rest a-seq)))

(defn insert [sorted-seq n]
  (loop [base []
         s sorted-seq]
    (cond
      (empty? s) (concat base [n])
      (> (first s) n) (concat base (cons n s))
      :else (recur (concat base [(first s)]) (rest s)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (reduce (fn[x y] (inc x)) 0 more))

(defn my-* [& more]
  (reduce * 1 more))

(defn pred-and [& more]
  (fn[x] (reduce (fn[f p] (and f (p x))) true more)))

(defn my-map-helper [seqs a-seq]
  (loop [init []
         base seqs
         new a-seq]
    (cond
      (empty? new) (concat init base)
      (empty? base) (recur (concat init [[(first new)]]) [] (rest new))
      :else (recur (concat init [(concat (first base) [(first new)])]) (rest base) (rest new)))))

(defn my-map [f & more]
  (reduce (fn[x p] (concat x [(apply f p)])) [] (reduce my-map-helper [] more)))
