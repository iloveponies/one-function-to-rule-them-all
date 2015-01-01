(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc x] (str acc " " x)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (let [fst (first a-seq)
          rst (rest a-seq)]
      (reduce (fn [acc e] (conj acc x e)) [fst] rst))))

(defn my-count [a-seq]
  (reduce (fn [c _] (inc c)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc x] (cons x acc)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[mmin mmax] x] [(min mmin x) (max mmax x)])
          [(first a-seq) (first a-seq)]
          a-seq))

(defn insert [sorted-seq n]
  (let [cmp (fn [x] (<= x n))]
    (concat (take-while cmp sorted-seq)
            (list n)
            (drop-while cmp sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set x]
                 (if (contains? a-set x)
                   (disj a-set x)
                   (conj a-set x)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& xs]
  (count xs))

(defn my-* [& xs]
  (reduce * xs))

(defn pred-and [& ps]
  (fn [x] (reduce (fn [acc p] (and acc (p x))) true ps)))

(defn my-map
  ([f a-seq] (reduce (fn [acc x] (conj acc (f x))) [] a-seq))
  ([f a-seq & more] (loop [acc []
                           args (cons a-seq more)]
                     (if (empty? (first args))
                       acc
                       (recur
                         (conj acc (apply f (my-map first args)))
                         (my-map rest args))))))
