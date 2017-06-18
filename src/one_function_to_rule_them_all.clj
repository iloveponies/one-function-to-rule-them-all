(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (let [helper (fn [coll elem] (conj coll x elem))]
      (reduce helper [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [length (fn [len e] (inc len))]
    (reduce length 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reversed (fn [coll elem] (cons elem coll))]
    (reduce reversed [] a-seq)))

(defn min-max-element [a-seq]
  (let [minmax (fn [coll elem]
                 (cond
                   (< elem (first coll)) (assoc coll 0 elem)
                   (> elem (second coll)) (assoc coll 1 elem)
                   :else coll))]
    (reduce minmax [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [a-seq    sorted-seq
         result   []
         n-added? false]
    (cond
      n-added? (reduce conj result a-seq)
      (empty? a-seq) (conj result n)
      (< n (first a-seq)) (recur a-seq (conj result n) true)
      :else (recur (rest a-seq) (conj result (first a-seq)) false))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [helper (fn [coll elem]
                 (if (contains? coll elem)
                   (disj coll elem)
                   (conj coll elem)))]
    (reduce helper #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & more] (reduce * x more)))

(defn pred-and
  ([] (fn [e] true))
  ([pred & more] (fn [e]
              (let [all-true (fn [p1 p2] (and p1 (p2 e)))]
                (reduce all-true (pred e) more)))))

(defn my-map [f a-seq]
  [:-])
