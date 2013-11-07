(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (empty? a-seq) ""
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (empty? a-seq) []
  (reduce (fn [y z] (if (empty? y)
                      (conj y z)
                      (conj y x z)))
          [] a-seq))

(defn my-count [a-seq]
  (empty? a-seq) 0
  (reduce (fn [n x] (if (empty? #{x})
                      n
                      (inc n)))
            0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    a-seq
  [(apply min a-seq)
   (apply max a-seq)]))

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq) (list n)
   (< n (first sorted-seq)) (cons n sorted-seq)
   :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))


(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce (fn [x e] (if (contains? x e)
                      (disj x e)
                      (conj x e)))
          #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [z] true))
  ([x] (fn [z] x z))
  ([x y] (fn [z] (and (x z) (y z))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn my-map [f a-seq]
  [:-])
