(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
      (reduce #(str % " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) a-seq
      (reduce #(conj % x %2) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [count elem]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %) [] a-seq))

(defn min-max-element [a-seq]
  [(reduce #(if (< % %2) % %2) a-seq)
   (reduce #(if (> % %2) % %2) a-seq) ])

(defn insert [sorted-seq n]
  (concat (take-while #(< % n) sorted-seq)
          (cons n (drop-while #(< % n) sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce #(if (% %2) (disj % %2) (conj % %2)) #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& x]
  (my-count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [a] true))
  ([x] (fn [a] (x a)))
  ([x y] (fn [a] ( and (x a) (y a))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn pairs [a-seq b-seq]
  (second (reduce (fn [[x acc] y]
                    [(rest x) (if (coll? (first x))
                                (conj acc (conj (first x) y))
                                (conj acc [(first x) y])) ])
            [a-seq []]
            b-seq)))

(defn my-map
  ([f a-seq] (reduce #(conj % (f %2)) [] a-seq))
  ([f a-seq b-seq] (reduce (fn [a [x y]] (conj a (f x y)))
                           []
                           (pairs a-seq b-seq)))
  ([f a-seq b-seq & more] (reduce (partial my-map f) (my-map f a-seq b-seq) more)))


