(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reverse (reduce #(conj %1 x %2) nil a-seq))))

(defn my-count [a-seq]
  (let [cnt (fn [acc _] (inc acc))]
    (reduce cnt 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) [] a-seq))

(defn min-max-element [a-seq]
  (let [f (fn [v n] [(min (first v) n) (max (last v) n)])]
    (reduce f [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (let [pred #(< % n)
        before (take-while pred sorted-seq)
        after (drop-while pred sorted-seq)]
    (concat before [n] after)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] x)
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce my-* (my-* x y) more)))

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])