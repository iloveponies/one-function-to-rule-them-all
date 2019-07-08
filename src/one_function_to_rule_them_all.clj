(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat `() a-seq))

(defn str-cat [a-seq]
  (cond
    (empty? a-seq) ""
    :else (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [count _] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (loop [acc [n] s sorted-seq]
    (cond
      (empty? s) acc
      :else (let [new-acc (if (< n (first s))
                            (conj acc (first s))
                            (conj (vec (drop-last acc)) (first s) n))]
              (recur new-acc (rest s))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-* [& x]
  (cond
    (= 0 (count x)) 1
    :else (reduce * 1 x)))

(defn pred-and [& p]
  (cond
    (= 0 (count p)) (fn [x] true)
    (= 1 (count p)) (fn [x] (p x))
    :else (reduce pred-and (fn [x] (and ((nth p 0) x) ((nth p 1) x))) (drop 2 p))))

(defn reorder-p-helper [& more]
  (partition (count more) (apply interleave more)))

(defn my-map [f & more]
  (cond
    (= 1 (count more)) (reduce #(conj %1 (f %2)) [] (first more))
    :else (reduce #(conj %1 (apply f %2)) [] (apply reorder-p-helper more))))
