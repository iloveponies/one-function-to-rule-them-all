(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (let [concat-with-spaces (fn [s1 s2] (str s1 " " s2))]
    (if (empty? a-seq)
      ""
      (reduce concat-with-spaces a-seq))))

(defn my-interpose [x a-seq]
  (let [concat-with-interpose (fn [s1 s2]
                                (conj s1 x s2))]
    (rest (reduce concat-with-interpose [] a-seq))))

(defn my-count [a-seq]
  (let [count-elements (fn [counter elem]
                         (inc counter))]
    (reduce count-elements 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverse-elements (fn [e1 e2]
                           (conj e1 e2))]
    (reduce reverse-elements '() a-seq)))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (let [min-val (reduce min a-seq)]
      (let [max-val (reduce max a-seq)]
        [min-val max-val]))))

(defn insert [sorted-seq n]
  (if
    (empty? sorted-seq) [n]
    (let [x (first sorted-seq)]
      (if (< x n)
        (cons x (insert (rest sorted-seq) n))
        (cons n (cons x (rest sorted-seq)))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [t-set elem]
    (if (contains? t-set elem)
      (disj t-set elem)
      (conj t-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x]   (* x -1))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

; since reducing * already provides the expected functionality there's
; no need to write conditional code based on number of arguments
(defn my-* [& args]
  (reduce * args))

(defn pred-and [& args]
  (fn [x]
    (let [nargs (count args)]
      (if (zero? nargs) true
      (reduce (fn [y f] (and y (f x))) '() args)))))

(defn my-map [f a-seq]
  [:-])
