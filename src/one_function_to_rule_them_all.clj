(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (rest (reduce (fn [a b] (conj a x b)) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [a elem] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [revseq elem] (cons elem revseq)) [] a-seq))

(defn min-max-element [a-seq]
  (let [minelem (reduce min a-seq)
        maxelem (reduce max a-seq)]
    [minelem maxelem]))

(defn insert [sorted-seq n]
  (loop [newseq []
         sorted-seq sorted-seq
         n n]
    (cond
      (empty? sorted-seq) (concat newseq (list n))
      (< n (first sorted-seq)) (concat newseq (cons n sorted-seq))
      :else (recur (conj newseq (first sorted-seq)) (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce (fn [sorted-seq elem] (insert sorted-seq elem)) [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [set elem]
                 (if (contains? set elem)
                   (disj set elem)
                   (conj set elem)))]

    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] (boolean true)))
  ([p] (fn [x] (p x)))
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more]
   (reduce pred-and (pred-and p q) more)))

(defn my-map [f a-seq]
  [:-])