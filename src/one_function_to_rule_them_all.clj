(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b))
            a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [a b]
              (if (empty? a)
                [b]
                (conj (conj a x) b)))
            []
            a-seq)))

(defn my-count [a-seq]
  (if (empty? a-seq)
    0
    (reduce (fn [a b] (inc a))
            a-seq)))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    ()
    (reduce (fn [a b] (cons b a))
            '()
            a-seq)))

(defn min-max-element [a-seq]
  (reduce (fn [[a b] x] [(min a x) (max b x)])
          [(first a-seq) (first a-seq)]
          a-seq))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (list n)
    (if (< n (first sorted-seq))
      (cons n sorted-seq)
      (cons (first sorted-seq)
            (insert (rest sorted-seq) n)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more] (reduce pred-and (pred-and p q) more)))

(defn my-map
  ([f a]
   (my-map (fn [x y] (f x))
           a
           (repeat (count a) nil)))
  ([f a b]
   (loop [acc []
          a a
          b b]
     (if (or (empty? a) (empty? b))
       acc
       (recur (conj acc (f (first a) (first b)))
              (rest a)
              (rest b)))))
  ([f a b & seqs]
   (my-map (fn [x y]
             (apply f (cons x y)))
           a
           (apply my-map (cons vector (cons b seqs))))))
