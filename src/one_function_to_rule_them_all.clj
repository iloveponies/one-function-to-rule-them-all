(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) [] a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (let [smaller (take-while (partial > n) sorted-seq)
        larger (drop-while (partial > n) sorted-seq)]
    (concat smaller [n] larger)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [s e]
                 (if (contains? s e) (disj s e)
                     (conj s e)))]
      (reduce toggle #{} a-seq)))
(defn minus
  ([x] (* x -1))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (* x y (reduce * 1 more))))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p & qs]
   (fn [x] (let [tests (for [q qs] (q x))]
            (and (p x) (every? identity tests))))))

(defn my-map [f & seqs]
  (apply map f seqs))
