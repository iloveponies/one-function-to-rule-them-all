(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  "Takes a sequence of strings and joins them with spaces"
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq))))


(defn my-count [a-seq]
  (let [counter (fn [count _] (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])


(defn insert [sorted-seq n]
  (if
    (or (empty? sorted-seq) (< n (first sorted-seq)))
      (cons n sorted-seq)
      (cons (first sorted-seq) (insert (rest sorted-seq) n))))


(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (* x -1))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-* [& x]
  (reduce * x))

(defn pred-and [& preds]
  (fn [x] (reduce (fn [acc elem] (and acc (elem x))) true preds)))

(defn my-map [f a-seq]
  [:-])


(my-reverse [1 2 3])


(min-max-element [2 7 3 15 4])

(insert '(1 2 3 5 6 7) 4)

(insertion-sort [1 3 2 5 9 2])

(parity '(1 2 3 3 4 ))


(minus 1)

(filter (pred-and) [1 0 -2])

(defn foo [x] true)

(foo 1)






