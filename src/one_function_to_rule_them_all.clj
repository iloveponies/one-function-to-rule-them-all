(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [a b] (conj a x b)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [a _] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [l x] (cons x l)) [] a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (let [a (first a-seq)
          minmax (fn [[low high] x]
                   (cond
                     (< x low)  [x high]
                     (> x high) [low x]
                     :else      [low high]))]
      (reduce minmax [a a] (rest a-seq)))))

(defn insert [sorted-seq n]
  (let [test (fn [x] (< x n))
        head (take-while test sorted-seq)
        tail (drop-while test sorted-seq)]
    (concat head [n] tail)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x]   (* x -1))
  ([x y] (- x y)))

(defn count-params [& xs]
  (reduce (fn [n _] (inc n)) 0 xs))

(defn my-* [& xs]
  (reduce * xs))

(defn pred-and [& xs]
  (fn [x] (reduce (fn [a b] (and a (b x))) true xs)))

(defn my-map [f a-seq & seqs]
  (let [multifirst (fn [seqs]
                     (reduce (fn [fs [f]] (conj fs f)) [] seqs))
        multirest  (fn [seqs]
                     (reduce (fn [rs a-seq] (conj rs (rest a-seq))) [] seqs))]
    (loop [xs []
           seqs (cons a-seq seqs)]
      (if (every? (complement empty?) seqs)
        (recur (conj xs (apply f (multifirst seqs))) (multirest seqs))
        xs))))
