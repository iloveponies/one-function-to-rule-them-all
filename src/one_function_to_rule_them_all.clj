(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [a b] (conj a x b)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [counter x] (inc counter)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [res x] (cons x res)) '() a-seq))

(defn min-max-element [a-seq]
  (let [helper (fn [[mini maxi] x]
                 [(min mini x) (max maxi x)])]
    (reduce helper [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (let [evaluate (fn [x] (< x n))
        head (take-while evaluate sorted-seq)
        tail (drop-while evaluate sorted-seq)]
    (concat head [n] tail)))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
    (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more]
   (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p & more] (fn [x] (reduce (fn [acc a] (and acc (a x))) (p x) more))))

(defn my-map [f a-seq]
  [:-])
