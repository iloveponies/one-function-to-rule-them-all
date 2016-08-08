(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [s1 s2] (str s1 " " s2)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (reduce (fn [b-seq item] (conj (conj b-seq x) item)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [count e] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [x y] (conj x y)) () a-seq))

(defn min-max-element [a-seq]
    [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (concat (filter (fn [x] (< x n)) sorted-seq) (concat [n] (filter (fn [x] (>= x n)) sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [x y] (if (contains? x y)
                      (disj x y)
                      (conj x y)))
          (set nil) a-seq))

(defn minus
    ([x] (- 0 x))
    ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-* [& x]
  (reduce * 1 x))

(defn pred-and
  ([] (fn [x] true))
  ([a] (fn [x] (a x)))
  ([a b] (fn [x] (and (a x) (b x))))
  ([a b & abc] (reduce pred-and (pred-and a b) abc)))

(defn my-map [f a-seq]
  [:-])
