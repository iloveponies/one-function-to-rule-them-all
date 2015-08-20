(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce
    (fn [a b]
      (if (empty? a)
        (conj a b)
        (conj a x b))) 
    []
    a-seq))

(defn my-count [a-seq]
  (reduce (fn [a b] (if (nil? b) a (inc a))) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) (cons n nil)
    (< (first sorted-seq) n) (cons (first sorted-seq)
                                   (insert (rest sorted-seq) n))
    :else (cons n sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce
    (fn [a-set a]
      (if (contains? a-set a)
        (disj a-set a)
        (conj a-set a))) 
    #{}
    a-seq))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x & more] (reduce (fn [a b] (if (nil? b) a (inc a))) 1 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (constantly true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & ps] (reduce pred-and (pred-and p1 p2) ps)))

(defn my-map
  ;; TODO xs ys zs & xss
  ([f xs]
    (if (empty? xs) 
      '()
      (cons (f (first xs)) (my-map f (rest xs)))))
  ([f xs ys]
    (if (or (empty? xs) (empty? ys))
      '()
      (cons (reduce f [(first xs) (first ys)])
            (my-map f (rest xs) (rest ys)))))
  ([f xs ys zs]
    (if (or (empty? xs) (empty? ys) (empty? zs))
      '()
      (cons (f (first xs) (first ys) (first zs))
            (my-map f (rest xs) (rest ys) (rest zs))))))
