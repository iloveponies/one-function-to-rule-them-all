(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
  (reduce (fn [s1 s2] (str s1 " " s2)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (if (> (count a-seq) 1)
      (reduce (fn [z y] (if (and (vector? z) (> (count z) 1))
        (conj z x y)
        (conj [z] x y)))
          a-seq)
    a-seq)))

(defn my-count [a-seq]
  (if (empty? a-seq)
    0
  (reduce (fn [x y] (inc x)) a-seq)))

(defn my-reverse [a-seq]
  (let [reverse (fn [new x]
    (cons x new))]
    (reduce reverse '() a-seq)))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (let [helper (fn [newseq n sorted-seq]
    (cond
      (empty? sorted-seq) (concat newseq [n])
      (< n (first sorted-seq)) (concat newseq [n] sorted-seq)
      :else (recur (concat newseq [(first sorted-seq)]) n (rest sorted-seq))))]
    (helper '() n sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
    (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))]
  (reduce toggle '#{} a-seq)))

(defn minus ([x] (* x -1))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & more]
    (reduce * x more)))

(defn pred-and
  ([] (fn[i]true))
  ([x] x)
  ([x y] (fn[i](and (x i) (y i))))
  ([x y & more]
    (fn[i] (and (x i) (y i) (reduce (fn[acc x](and acc x)) (map (fn[f](f i)) more) )))))

(defn my-map [f & a-seq]
  (if (<=(count  a-seq) 1)
    (map f (first a-seq))
    (map (fn[x](apply f x))(partition (count a-seq) (apply interleave a-seq)))))
