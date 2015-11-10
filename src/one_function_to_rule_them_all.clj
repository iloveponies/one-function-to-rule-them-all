(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce (fn[x y] (str x " "  y)) a-seq)
    ))

(defn my-interpose [x a-seq]
    (if (empty? a-seq) []
    (drop 1 (reduce (fn[y z] (conj y x z)) [] a-seq))
    ))

(defn my-count [a-seq]
  (reduce (fn[x y] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn[x y] (concat [y] x)) [] a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq) []
  (reduce (fn[[mi ma] y] [(min mi y) (max ma y)])
          [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (loop [acc []
         ns n
         [f & fs] sorted-seq]
      (cond
       (nil? f) (conj acc n)
       (< n f) (concat acc [n f] fs)
       :else (recur (conj acc f) (dec n) fs)
    )))

(defn insertion-sort [a-seq]
  (reduce (fn[x y] (insert x y)) [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem)  (conj a-set elem)))

(defn parity [a-seq]
  (reduce (fn[x y] (toggle x y)) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& xs]
  (my-count xs))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & xs] (reduce * x xs)))

(defn pred-and
  ([](fn [x] true))
  ([p] (fn[x] (p x) ))
  ([p & ps]  (fn[x] (reduce (fn[y z] (and y (z x))) (p x)  ps))))

; within clojure (map vector xs) equals zipN
(defn my-map
  ([f & as]
      (let [xs (apply map vector as)]
      (map (fn[x] (apply f x)) xs))
  ))
