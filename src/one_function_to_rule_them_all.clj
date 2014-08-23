(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc x] (str acc " " x)) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [acc e] (conj acc x e)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[l h] x] [(min l x) (max h x)]) [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (loop [acc []
         a-seq sorted-seq]
    (cond
     (empty? a-seq) (seq (conj acc n))
     (< n (first a-seq)) (concat (conj acc n) a-seq)
     :else (recur (conj acc (first a-seq)) (rest a-seq)))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce (fn [acc x]
            (if (contains? acc x)
              (disj acc x)
              (conj acc x))) #{} a-seq))

(defn minus
  ([x]   (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (reduce (fn [acc _] (inc acc)) 0 more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (* x y) more)))

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
