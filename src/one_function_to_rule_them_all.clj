(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce
      (fn [vect item] (conj vect x item))
      (vector (first a-seq))
      (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    []
    (reduce
      (fn [a b] (cons b a))
      (list (first a-seq))
      (rest a-seq))))

(defn min-max-element [a-seq]
  (reduce
    (fn [[a b] elem] [(min a elem) (max b elem)])
    [(first a-seq) (first a-seq)]
    a-seq))

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) (list n)
    (< n (first sorted-seq)) (cons n sorted-seq)
    :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

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

(defn count-params [& x]
  (count x))

(defn my-* [& x]
  (reduce * 1 x))

(defn pred-and [& x]
  (fn [elem]
    (loop [preds x]
      (cond
        (empty? preds) true
        ((first preds) elem) (recur (rest preds))
        :else false))))

(defn my-map [f a-seq]
  [:-])