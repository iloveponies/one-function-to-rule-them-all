(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (reduce (fn [a b]
            (if (empty? a)
              [b]
              (conj a x b))) [] a-seq))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b]
            (if (empty? a)
              [b]
              (cons b a))) [] a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [a b]
            (if (empty? a)
              [b b]
              [(min (first a) b) (max (second a) b)])) [] a-seq))

(defn insert [sorted-seq n]
  (let [pred? (fn [item] (<= item n))]
    (concat (take-while pred? sorted-seq) (cons n (drop-while pred? sorted-seq)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [parities item]
            (if (contains? parities item)
              (disj parities item)
              (conj parities item)
              )) #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y))
  )

(defn count-params
  ([& more] (reduce (fn [a b] (inc a)) 0 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & more] (reduce * x more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred] (fn [x] (pred x)))
  ([pred & preds] (fn [x] (reduce (fn [a b] (and a (b x))) (pred x) preds) ))
  )

(defn my-map
  ([f a-seq] (reduce (fn [a item] (conj a (f item))) [] a-seq))
  ([f a-seq & seqs] (reduce (fn [result f-applied-seq]
                              ()) [] seqs)))