(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (str (reduce (fn [acc elem] (str acc (if (nil? acc) "" " ") elem)) a-seq))))

(defn my-interpose [x a-seq]
  (reduce (fn [acc elem] (if (empty? acc) (vector elem) (conj acc x elem))) [] a-seq))

(defn my-count [a-seq]
  (reduce (fn [acc elem] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc elem] (cons elem acc)) [] a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [acc elem] [(min (first acc) elem) (max (last acc) elem)]) [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (let [smaller (vec (take-while (fn [x] (< x n)) sorted-seq))
        bigger  (vec (drop-while (fn [x] (< x n)) sorted-seq))]
    (concat (conj smaller n) bigger)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                (if (contains? a-set elem)
                  (disj a-set elem)
                  (conj a-set elem)))]
    (reduce (fn [acc elem] (toggle acc elem)) #{} a-seq)))

(defn minus
  ([x]   (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([& x] (my-count x)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))


(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p & more]
    (reduce
      (fn [prev curr]
        (fn [x] (and (prev x) (curr x))))

      (fn [x] (p x))

      (reverse more))))


(defn my-map
  ([f seq] (reduce f seq))
  ([f seq & seqs] (reduce (fn [prev curr] (reduce f curr)) (concat seq seqs))))
