(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
   (reduce str ( interpose " " a-seq)))

(defn my-interpose [x xs]
  (rest (reduce (fn [a b] (conj a x b)) [] xs)))

(defn my-count [a-seq]
  (let [counter (fn [acc e] (inc acc))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  (let [minmaxer (fn [[mi ma] e]
                 [(min mi e) (max ma e)])]
    (reduce minmaxer [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (let [lessthan-n (fn [x] (< x n))]
    (concat (take-while lessthan-n sorted-seq) (cons n '())
            (drop-while lessthan-n sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                    (disj a-set elem)
                    (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (* x -1))
  ([x y] (- x y)))

(defn count-params [& params]
  (my-count params))

(defn my-*[& params]
  (reduce * 1 params))

(defn pred-and [& params]
  (fn [x]
    (reduce
     (fn [p f] (and p (f x)))
     true
     params)))

(defn my-map [f a-seq]
  [:-])
