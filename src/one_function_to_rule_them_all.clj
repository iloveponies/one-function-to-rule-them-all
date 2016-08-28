(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (let [stringer (fn [x y]
                   (str x " " y))]
    (if (empty? a-seq)
      ""
      (reduce stringer a-seq))))

(defn my-interpose [x a-seq]
  (let [interposer (fn [a b]
                     (conj a x b))]
    (rest (reduce interposer [] a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [acc _]
                  (inc acc))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [min-maxer (fn [acc x]
                    (let [minimum (min (first acc) x)
                          maximum (max (second acc) x)]
                      [minimum maximum]))]
    (reduce min-maxer [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (let [smaller-than-n (fn [x] (< x n))
        start (take-while smaller-than-n sorted-seq)
        end (drop-while smaller-than-n sorted-seq)]
    (concat start (cons n end))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* 
  ([] 1)
  ([x y] (* x y))
  ([x y & more]
    (reduce * (* x y) more)))

(defn pred-and 
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map [f a-seq]
  [:-])