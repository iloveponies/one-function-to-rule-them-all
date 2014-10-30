(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (reduce #(if (empty? %1)
             (conj %1 %2)
             (conj %1 x %2)) []  a-seq))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[amin amax] value] (vector (min amin value) (max amax value))) [Integer/MAX_VALUE Integer/MIN_VALUE] a-seq))

(defn insert [sorted-seq n]
  (cond (empty? sorted-seq) [n]
        (< n (first sorted-seq)) (cons n sorted-seq)
        :else (cons (first sorted-seq) 
                    (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce #(insert %1 %2) [] a-seq))

(defn parity [a-seq]
  (reduce #(if (contains? %1 %2) (disj %1 %2) (conj %1 %2)) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (fn [x] (reduce #(and %1 (%2 x)) (and (p1 x) (p2 x)) more))))

(defn my-map
  ([f a-seq] 
     (reduce (fn [accumulator value] (conj accumulator (f value))) 
             [] 
             a-seq))

  ([f a-seq b-seq & more]
    (let [colls (into [a-seq b-seq] more)]
      (reduce (fn [accumulator values] (conj accumulator (apply f values))) 
              []
              (partition (count colls) (apply interleave colls))))))
