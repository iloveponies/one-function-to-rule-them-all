(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (reduce #(if (= [] %1)
             [%2]
             (conj %1 x %2)) [] a-seq))

(defn my-count [a-seq]
  (reduce (fn [x y] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq ))

(defn min-max-element [a-seq]
  (reduce #(vector (min (first %1) %2) (max (second %1) %2))
          [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (cond (empty? sorted-seq)
        (cons n nil)
        (< n (first sorted-seq))
        (cons n sorted-seq)
        :else
        (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce #(insert %1 %2) '() a-seq))

(defn parity [a-seq]
  (reduce #(if (contains? %1 %2)
             (disj %1 %2)
             (conj %1 %2)) #{} a-seq))

(defn minus
 ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
    ([] (fn [x] true))
    ([y] (fn [x] (y x)))
    ([y z] (fn [x] (and (y x) (z x))))
    ([y z & more] (fn [x] (reduce #(and %1 (%2 x))
                                  (and (y x) (z x)) more))))


(defn my-map [f a-seq]
  [:-])

