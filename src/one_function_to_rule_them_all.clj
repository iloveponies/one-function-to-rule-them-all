(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (seq (reduce (fn [a b] (conj a x b)) [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [coll el] (cons el coll)) (list) a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[mn mx] n] [(min mn n) (max mx n)]) [(first a-seq) (first a-seq)] (rest a-seq)))

(defn insert [sorted-seq n]
  (loop [coll (seq sorted-seq)
         acc []]
    (if (or (nil? coll)
            (< n (first coll)))
      (concat acc (cons n coll))
      (recur (seq (rest coll))
             (conj acc (first coll))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set x]
  (if (a-set x)
    (disj a-set x)
    (conj a-set x)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (my-count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x)
                        (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map
  ([f a-seq] (seq (reduce (fn [coll x] (conj coll (f x))) [] a-seq)))
  ([f a-seq & more] "welp"))
