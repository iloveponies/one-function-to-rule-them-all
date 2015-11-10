(ns one-function-to-rule-them-all
  (:use clojure.repl))


(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [a b] (conj a x b)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [cnt _] (inc cnt)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [x y] (cons y x)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [x y] [(apply min (conj x y)) (apply max (conj x y))]) [] a-seq))

(defn insert [sorted-seq n]
  (loop [p-1 []
         p-2 sorted-seq]
    (let [checked-seq (apply conj p-1 (cons n p-2))]
      (if (apply <= checked-seq)
        checked-seq
        (recur (conj p-1 (first p-2)) (rest p-2))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))


(defn parity [a-seq]
  (let [toggle (fn [a-set elm]
                 (if (a-set elm)
                   (disj a-set elm)
                   (conj a-set elm)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [elm] true))
  ([p?] (fn [elm] (p? elm)))
  ([p1? p2?] (fn [elm] (and (p1? elm) (p2? elm))))
  ([p1? p2? & preds?] (reduce pred-and (pred-and p1? p2?) preds?)))


(defn my-map [f & colls]
  (let [map (fn [f coll]
              (reduce (fn [x y] (conj x (f y))) [] coll))]
    (if (some empty? colls)
      '()
      (cons
       (apply f (map first colls))
       (apply my-map f (map rest colls))))))
