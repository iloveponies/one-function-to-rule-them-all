(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (let [helper (fn [coll item]
                 (if (empty? coll)
                   (conj coll item)
                   (conj coll x item)))]
    (reduce helper [] a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [count _] (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [helper (fn [coll item] (concat [item] coll))]
    (reduce helper [] a-seq)))

(defn min-max-element [a-seq]
  [(reduce (fn [a b] (min a b)) a-seq) (reduce (fn [a b] (max a b)) a-seq)])

(defn insert [sorted-seq n]
  (let [head (take-while (fn [x] (< x n)) sorted-seq)]
    (concat head [n] (drop (count head) sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [helper (fn [a-set item]
                 (if (contains? a-set item)
                   (disj a-set item)
                   (conj a-set item)))]
    (reduce helper #{} a-seq)))


(defn minus
  ([x]   (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-* [& x]
  (let [len (count x)]
    (cond
      (= len 0) 1
      (= len 1) (first x)
      (= len 2) (* (first x) (last x))
      :else     (reduce * 1 x))))


(defn pred-and [& x]
  (let [len (count x)]
    (cond
      (= len 0) (fn [_] true)
      :else     (fn [y] (every? true? (map (fn [pred] (pred y)) x))))))

(defn my-map [f & several-seqs]
  (loop [acc []
         seqs several-seqs]
    (if (empty? (first seqs))
      acc
      (recur (conj acc (apply f (map first seqs))) (map rest seqs)))))
