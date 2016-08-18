(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce
          (fn [a b] (conj a x b))
          []
          a-seq)))

(defn my-count [a-seq]
  (reduce (fn [count e]
            (if (boolean e)
              (inc count)
              count))
          0
          a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [maxhelper (fn [a b]
                 (if (> a b)
                   a
                   b))]
    (let [minhelper (fn [a b]
                      (if (> a b)
                        b
                        a))]
      (vector (reduce minhelper a-seq)(reduce maxhelper a-seq)))))

(defn insert [sorted-seq n]
  (sort (conj sorted-seq n)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn parity [a-seq]
  (let [helper (fn [x [a b]]
                 (if (odd? b)
                   (conj x a)
                   x))]
    (reduce helper #{} (frequencies a-seq))))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params ([& more](count more)))

(defn my-*
  ([& more] (reduce * 1 more)))

(defn pred-and [& preds]
   (fn [& a-seq]
    (every? (fn [pred] (apply pred a-seq))
            preds)))

(defn my-map [f a-seq]
  [:-])
