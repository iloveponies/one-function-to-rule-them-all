(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(concat %1 [x %2]) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [a _] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(conj %1 %2) '() a-seq))

(defn min-max-element [a-seq]
  (vector (reduce min a-seq) (reduce max a-seq)))

(defn insert [sorted-seq n]
  (loop [seen []
         remaining sorted-seq]
    (cond
      (empty? remaining) (conj seen n)
      (< n (first remaining)) (concat (conj seen n) remaining)
      :else (recur (conj seen (first remaining)) (rest remaining)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (letfn [(in-out [a-set item]
            (if (contains? a-set item)
              (disj a-set item)
              (conj a-set item)))]
    (reduce in-out #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-* [& args]
  (reduce * 1 args))

;; (defn pred-and
;;   ([] (fn [x] true))
;;   ([pred] (fn [x] (pred x)))
;;   ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
;;   ([p1 p2 & ps] (fn [x] (and
;;                            (p1 x)
;;                            (p2 x)
;;                            (every? #(% x) ps)))))

(defn pred-and [& preds]
  (fn [x]
    (reduce #(and %1 (%2 x)) true preds)))

(defn my-map
  ([f a-seq] (reduce #(conj %1 (f %2)) [] a-seq))
  ([f a-seq & seqs]
    (->>
      (concat [a-seq] seqs)
      (apply interleave)
      (#(partition-all (inc (count seqs)) %))
      (reduce #(conj %1 (apply f %2)) []))))
