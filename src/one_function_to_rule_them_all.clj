(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (cond
   (empty? a-seq) ""
   :else (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce (fn [a b]
            (if (empty? a)(conj a b) (conj a x b))) [] a-seq))

(defn my-count [a-seq]
  (reduce (fn [x y] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [x y] (cons y x)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[a-min a-max] x]
            [(min a-min x) (max a-max x)])
          [(first a-seq) (first a-seq)]
          a-seq))

(defn insert [sorted-seq n]
  (cond (empty? sorted-seq) [n]
        (< n (first sorted-seq)) (cons n sorted-seq)
        :else (cons (first sorted-seq)
                    (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [a-set x] (if (contains? a-set x)
                          (disj a-set x)
                          (conj a-set x)))
          #{}
          a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& x]
  (my-count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
   (reduce (fn [p-old p-new]
             (fn [x] (and (p-old x) (p-new x))))
           (pred-and p1 p2) more)))

(defn my-map
  ([f a-seq] (reduce (fn [x y] (conj x (f y))) [] a-seq))
  ([f a-seq b-seq] (loop [acc []
                          la a-seq
                          lb b-seq]
                     (cond
                      (empty? la) acc
                      (empty? lb) acc
                      :else
                      (recur (conj acc (f (first la) (first lb)))
                             (rest la)
                             (rest lb)))))
  ([f a-seq b-seq & more] (reduce (fn [x y] (my-map f x y))
                                  (my-map f a-seq b-seq) more)))

