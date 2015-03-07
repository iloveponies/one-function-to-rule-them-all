(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn catspace [s coll]
  (str (str s " ") coll) )

(defn str-cat [a-seq]
  (if (empty? a-seq) (str "")
    (reduce catspace a-seq)))

(defn my-conj [coll y]
  (conj (conj coll 0 ) y))


(defn my-interpose [x a-seq]
  (let [my-conj (fn [coll y]
                 (conj (conj coll x ) y))]
    (if (empty? a-seq) a-seq
     (rest (reverse (reduce my-conj () a-seq))))))

(defn my-count [a-seq]
  (let [counter (fn [count e] (inc count))]
  (if (empty? a-seq) 0
    (reduce counter 0 a-seq))))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  (let [helper (fn [coll y] (if (> y (first coll))
                               (conj (rest coll) y)
                              (if (< y (second coll))
                                (reverse (conj (drop-last coll) y))
                                coll)
                              ))]
    (if (empty? a-seq) '[]
     (vec (reverse (reduce helper (repeat 2 (first a-seq)) a-seq))))))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq) (cons n sorted-seq)
   (if (< n (first sorted-seq)) (cons n sorted-seq)
     (cons (first sorted-seq) (insert (rest sorted-seq) n)))
    ))

(defn insertion-sort [a-seq]
  (if (empty? a-seq) '()
    (reduce insert () a-seq)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle (set ()) a-seq))

(defn minus ([x] (* -1 x))
  ([x y] (- x y))
  )

(defn count-params [& more]
  (let [counter (fn [count e] (inc count))]
    (reduce counter 0 more))
  )

(defn my-* ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more))
  )

(defn pred-and ([] (fn[x] true))
  ([p] p)
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more ]
   (reduce pred-and (pred-and p q) more)))

(defn my-map [f a-seq]
  [:-])
