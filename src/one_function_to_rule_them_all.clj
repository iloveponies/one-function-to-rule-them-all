(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (let [join(fn [a b] (str a " " b))]
    (if (empty? a-seq)
      ""
      (reduce join a-seq))))

(defn my-interpose [x a-seq]
  (let [interp(fn [a-seq b-seq] (if (empty? a-seq) (conj [] b-seq) (conj a-seq x b-seq)))]
      (reduce interp [] a-seq)))

(defn my-count [a-seq]
  (let [counter(fn [counter element] (+ counter 1))]
  (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce (fn [target-seq element] (cons element target-seq)) [] a-seq))

(defn min-max-element [a-seq]
  (let [min-max-helper (fn [[min-value max-value] x]
                         [(min min-value x) (max max-value x)])]
    (reduce min-max-helper [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (let [split-helper (fn [x] (< x n))]
    (concat
      (take-while split-helper sorted-seq)
      (cons n '())
      (drop-while split-helper sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [result-set x]
                 (if (contains? result-set x)
                   (disj result-set x)
                   (conj result-set x)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& params]
  (my-count params))

(defn my-* [& params]
  (reduce * 1 params))

(defn pred-and [& preds]
  (let [pred-helper (fn [x] (fn [prev-result pred] (and prev-result (pred x))))]
  (fn [x] (reduce (pred-helper x) true preds))))

(defn my-map [f a-seq]
  [:-])