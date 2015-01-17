(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b))  (first a-seq) (rest a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [a b] (conj a x b))  [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [x y] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [x y] (concat [y] x)) [] a-seq))

(defn min-max-element [a-seq]
  (let [a (first a-seq)
        init [a a]
        min-max (fn [mm x]
                  (cond
                   (< x (first mm))
                     (assoc mm 0 x)
                   (> x (last mm))
                     (assoc mm 1 x)
                   :else
                     mm))]
    (reduce min-max init (rest a-seq))))

(defn insert [sorted-seq n]
  (let [begin (take-while (fn [x] (< x n)) sorted-seq)
        c (count begin)
        end (drop c sorted-seq)]
    (concat begin [n] end)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (reduce (fn [x y] (inc x)) 0 more))

(defn my-* [& more]
  (reduce * 1 more))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more] (reduce pred-and (pred-and p q) more)))

(defn my-map [f a-seq]
  [:-])
