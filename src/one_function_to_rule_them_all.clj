(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))



(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))


(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [new-set set-item] (conj new-set x set-item)) [(first a-seq)] (rest a-seq))))


(defn my-count [a-seq]
  (let [count-items (fn [current-count element]
                      (inc current-count))]
    (reduce count-items 0 a-seq)))


(defn my-reverse [a-seq]
  (let [reverse-set (fn [new-set current-item]
                      (conj new-set current-item))]
    (reduce reverse-set '() a-seq)))


(defn min-max-element [a-seq]
  (let [min-max (fn [[min max] current-item]
                  (if (< current-item min)
                    (if (> current-item max)
                      [current-item current-item]
                      [current-item max])
                    (if (> current-item max)
                      [min current-item]
                      [min max])))]
    (reduce min-max [100 -100] a-seq)))


(defn insert [sorted-seq n]
  (let [seq-first (first sorted-seq)
        seq-rest (rest sorted-seq)]
    (if (empty? sorted-seq)
      [n]
      (if (> seq-first n)
        (cons n sorted-seq)
        (cons seq-first (insert seq-rest n))))))


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
  (count more))


(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (if (and (p1 x) (p2 x)) true false)))
  ([p1 p2 & more]
   (reduce pred-and (pred-and p1 p2) more)))


; my-map is no working for all cases
(defn my-map
  ([f a-seq] (loop [result []
                    function f
                    a-seq a-seq]
               (if (empty? a-seq)
                 result
                 (recur (conj result (function (first a-seq)))
                  f
                  (rest a-seq)))))
  ([f a-seq & more]
   (pr a-seq more)))
