(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (drop 1 (interleave (repeat x) a-seq)))

(defn my-count [a-seq]
  (if (empty? a-seq)
    0
    (let [counter (fn [count a-seq]
                 (inc count))]
      (reduce counter 0 a-seq))))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  (let [helper (fn [mn mx a-seq])]
    (list (apply min a-seq) (apply max a-seq))))

(defn insert [sorted-seq n]
  (let [helper (fn [typ acc sorted-seq n]
                 (cond
                  (and (empty? sorted-seq) (not (= nil n))) (reverse (conj acc n))
                  (and (empty? sorted-seq) (= nil n)) (reverse acc)
                  (= nil n) (recur typ (conj acc (first sorted-seq)) (rest sorted-seq) nil)
                  (typ (first sorted-seq) n) (recur typ (conj acc (first sorted-seq)) (rest sorted-seq) n)
                  :else (recur typ (conj acc n) sorted-seq nil)))]
     (helper < () sorted-seq n)))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  [:-])

(defn minus
  ([x]  (- x))
  ([x y] (- x y)))

(defn count-params
  ([]  0)
  ([x] 1)
  ([x & more] (inc (count more))))

(defn my-*
  ([] (*))
  ([x] (* x))
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
