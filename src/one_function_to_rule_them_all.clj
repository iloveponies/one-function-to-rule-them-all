(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [s t] (str s " " t)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [s t] (conj s x t)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [counter elem] (inc counter)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[minimum maximum] elem]
    [(min minimum elem) (max maximum elem)]) [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) [n]
    (< n (first sorted-seq)) (cons n sorted-seq)
    :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (reduce (fn [counter elem] (inc counter)) 0 more))

(defn my-* [& more]
  (reduce * 1 more))

(defn pred-and [& preds]
  (fn [x]
    (reduce (fn [old pred] (and old (pred x))) true preds)))

(defn my-map [f & seqs]
  (loop [res []
         sqs seqs]
    (if (empty? (first sqs))
      res
      (recur (conj res (apply f (map first sqs))) (map rest sqs)))))
