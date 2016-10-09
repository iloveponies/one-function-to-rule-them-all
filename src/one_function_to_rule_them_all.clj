(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
 (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
 (reduce (fn [a b](str a " " b)) a-seq))
)

(defn my-interpose [x a-seq]
 (drop-last (reduce (fn [result current] (conj result current x)) [] a-seq)))

(defn my-count [a-seq]
(reduce (fn [cnt _] (inc cnt)) 0 a-seq))

(defn my-reverse [a-seq]
(reduce (fn [xs x] (cons x xs)) '() a-seq)
 )

(defn min-max-element [a-seq]
  (let [sorter (fn [[x y] e]
                  (vector (min x e) (max y e)))]
    (reduce sorter [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
(concat (take-while #(< % n) sorted-seq)
[n]
(drop-while #(< % n) sorted-seq)))

(defn insertion-sort [a-seq]
 (reduce insert [] a-seq))

(defn parity [a-seq]
 (let [set-adder (fn [s e](if (contains? s e) (disj s e) (conj s e)))]
 (reduce set-adder #{} a-seq))
)

(defn minus
  ([x] (* (- 1) x))
  ([x y] (- x y))
)

(defn count-params
  ([& more] (my-count more))
)

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce * (* x y) more)))


(defn pred-and
([] (fn [x] true))
([x](fn [x] x))
([x1 y1] (fn [x] (and (x1 x) (y1 x))))
([x y & more]
(reduce pred-and (pred-and x y) more)))

(defn my-map [f a-seq]
  [:-])
