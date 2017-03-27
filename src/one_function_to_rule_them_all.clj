(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
 (reduce concat
         []
         a-seq))

(defn str-cat [a-seq]
 (if (empty? a-seq)
  ""
  (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
 (drop 1 (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
 (reduce (fn [accum value] (inc accum))
         0
         a-seq))

(defn my-reverse [a-seq]
 (reduce (fn [accum value] (cons value accum))
         '()
         a-seq))

(defn min-max-element [a-seq]
 (reduce #(let [[curr-min curr-max] %1
                number-to-evaluate %2]
            [(min curr-min number-to-evaluate) (max curr-max number-to-evaluate)])
         [(first a-seq) (first a-seq)]
         a-seq))

(defn insert [sorted-seq n]
 (sort (conj sorted-seq n)))

(defn insertion-sort [a-seq]
 (reduce #(insert %1 %2)
         '()
         a-seq))

(defn toggle [a-set elem]
 (if (contains? a-set elem)
  (disj a-set elem)
  (conj a-set elem)))

(defn parity [a-seq]
 (reduce #(toggle %1 %2)
         #{}
         a-seq))

(defn minus
 ([x] (- x))
 ([x y] (- x y)))

(defn count-params [& params]
 (count params))

(defn my-*
 ([] 1)
 ([x] x)
 ([x y] (* x y))
 ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
 ([] (fn [x] true))
 ([p] (fn [x] (p x)))
 ([p q] (fn [x] (and (p x) (q x))))
 ([p q & more] (reduce pred-and (pred-and p q) more)))

(defn my-map-helper [f a-seq]
 (seq (reduce #(conj %1 (f %2))
              []
              a-seq)))

(defn my-map
 ([f coll] (my-map-helper f coll))
 ([f coll & more-colls] (let [collections (cons coll more-colls)]
                         (my-map (partial apply f)
                                 (partition (count collections) (apply interleave collections))))))
