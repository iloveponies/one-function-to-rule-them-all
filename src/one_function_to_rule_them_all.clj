(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str % " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj % x %2) [] a-seq)))

(defn my-count [a-seq]
  (reduce #(do %& (inc %)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(conj % %2) () a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[a b] c] [(min a c)(max b c)]) [Double/MAX_VALUE Double/MIN_VALUE] a-seq))

(defn insert [sorted-seq n]
  (let [[b a] (split-with #(< % n) sorted-seq)]
    (lazy-cat b [n] a)))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (reduce #(if (contains? % %2) (disj % %2) (conj % %2)) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more](count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (constantly true))
  ([p] #(p %))
  ([p1 p2] #(and (p1 %) (p2 %)))
  ;
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map
  ([f a-seq] (map f a-seq))
  ;If you want lazy-seqs, don't make f vec
  ([f a-seq & more] (apply map f (cons a-seq more))))
