(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b]
              (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
    (reverse (rest (reduce (fn [a b]
              (conj a b x))
            '() a-seq))))

(defn my-count [a-seq]
  (reduce (fn [cnt a] (inc cnt)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc a] (conj acc a)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [minmax a]
            (let [[minel maxel] minmax]
              [(min minel a) (max maxel a)]))
          [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (cond
   (nil? n) sorted-seq
   (empty? sorted-seq) (list n)
   (< (first sorted-seq) n) (cons (first sorted-seq) (insert (rest sorted-seq) n))
   :else (cons n (insert sorted-seq nil))))


(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce (fn [acc a]
            (if (contains? acc a)
              (disj acc a)
              (conj acc a))) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)                         ; one parameter
  ([x y] (* x y))        ; two parameters
  ([x y & more]                   ; more than two parameters
    (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn[x] true))
  ([p1] (fn[x] (p1 x)))
  ([p1 p2] (fn[x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
   (reduce pred-and (pred-and p1 p2) more)))

(defn fmap [f seq]
  (if (empty? seq)
    []
    (cons (f (first seq)) (fmap f (rest seq)))))

(defn rotate [seq]
  (if (every? empty? seq)
    []
    (cons (fmap first seq) (rotate (fmap rest seq)))))

(defn my-map
  ([f seq] (fmap f seq))
  ([f seq & more]
   (let [rotated (rotate (cons seq more))]
     (fmap (fn [s] (apply f s)) rotated))))
