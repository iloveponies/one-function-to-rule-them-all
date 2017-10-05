(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [b-seq x] (str b-seq " " x)) a-seq)))

(defn my-interpose2 [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [b-seq y] (conj b-seq x y)) a-seq)))

(defn my-interpose [x a-seq]
  (cond
    (empty? a-seq)
      []
    (= 1 (count a-seq))
      a-seq
    :else
      (reduce (fn [b-seq y] (conj b-seq x y)) a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                  (if e (inc count) count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    []
    (reduce cons a-seq)))

(defn min-max-element [a-seq]
  [:-])

(defn insert [sorted-seq n]
  [:-])

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  [:-])

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [a] a))
  ([x] (fn [a] (x a)))
  ([x y] (fn [a] (and (x a) (y a))))
  ([x y & more] (reduce (fn [a] (and (x a) (y a))) more)))

(defn my-map [f a-seq]
  [:-])
