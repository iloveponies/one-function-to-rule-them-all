(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc s] (str acc " " s)) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [acc elem] (conj acc x elem)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [cnt e] (inc cnt)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc e] (cons e acc)) '() a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (cons n sorted-seq)
    (if (< (first sorted-seq) n)
      (cons (first sorted-seq) (insert (rest sorted-seq) n))
      (cons n sorted-seq))))

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

(defn count-params [& params]
  (count params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
   (fn [x] (reduce (fn [acc p3] (and acc (p3 x))) (and (p1 x) (p2 x)) more))))

(defn my-map
  ([f coll]
   (if (empty? coll)
    '()
     (cons (f (first coll))
           (my-map f (rest coll)))))
  ([f coll & more]
   (let [colls (concat [coll] more)]
     (if (some empty? colls)
       '()
       (cons (apply f (my-map first colls))
             (apply my-map f (my-map rest colls)))))))

