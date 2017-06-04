(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (if (> 2 (count a-seq))
    a-seq
    (rest (reduce (fn [y z] (conj y x z)) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [n e] (inc n)) 0 a-seq))

(defn my-reverse [a-seq]
  (if (> 2 (count a-seq))
    a-seq
    (reduce (fn [x y] (conj x y)) '() a-seq)))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (concat (take-while (fn [x] (< x n)) sorted-seq)
          [n]
          (drop-while (fn [x] (< x n)) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y & more] (reduce my-* (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p q & more] (reduce pred-and
                        (fn [x] (and (p x) (q x)))
                        more)))
(defn my-map [f a-seq]
  [:-])

;Does not work at this point
;(defn my-map;(map f)(map f coll)(map f c1 c2)(map f c1 c2 c3)(map f c1 c2 c3 & colls)
; ([f a-seq]
;  (if (seq a-seq)
;   (lazy-seq
;    (cons (f (first a-seq))
;          (my-map f (rest a-seq))
;  )))
;  [f a-seq b-seq] (reduce f [] a-seq)
;  [f a-seq b-seq c-seq & more] (reduce f [] a-seq)
;))
