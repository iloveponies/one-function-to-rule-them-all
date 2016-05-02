(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [str1 str2] (str str1 " " str2)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (rest (reduce (fn [fst snd]
            (conj fst x snd)) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc item] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc item] (conj acc item)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [acc elem]
            (assoc acc 0 (min (first acc) elem))
            (assoc acc 1 (max (last acc) elem))) [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq)
      (conj '() n)
    (< n (first sorted-seq))
      (cons n sorted-seq)
    :else
      (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce (fn [acc item] (insert acc item)) '() a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [elem] true))
  ([pred] pred)
  ([pred1 pred2] (fn [elem] (and (pred1 elem) (pred2 elem))))
  ([pred1 pred2 & more] (reduce pred-and (pred-and pred1 pred2) more)))

(defn my-map [f a-seq]
  [:-])
