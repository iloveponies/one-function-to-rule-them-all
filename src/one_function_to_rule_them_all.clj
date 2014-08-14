(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [ret v] (str ret " " v)) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [ret v] (conj ret v x)) '() (reverse a-seq))))

(defn my-count [a-seq]
  (reduce (fn [ret v] (inc ret)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [ret v] (conj ret v)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[minv maxv] v] [(min minv v) (max maxv v)]) [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq) (cons n '())
   (<= n (first sorted-seq)) (cons n sorted-seq)
   (>= n (first (reverse sorted-seq))) (reverse (cons n (reverse sorted-seq)))
   :else (first (reduce (fn [[acc, added] v]
                   (if (and (false? added) (<= n v))
                     [(reverse (cons v (cons n (reverse acc)))), true]
                     [(reverse (cons v (reverse acc))), added])) ['(), false] sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce (fn [acc v]
            (if (odd? (my-count (filter (fn [x] (= x v)) a-seq)))
              (conj acc v)
              acc)) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-* [& x]
  (cond
   (nil? x) 1
   (= (count x) 1) (first x)
   (= (count x) 2) (* (first x) (second x))
   :else (reduce * 1 x)))

(defn pred-and [& preds]
  (fn [x]
    (cond
     (nil? preds) true
     (not (seq? preds)) (preds x)
     :else (reduce (fn [ret f]
                     (if (false? ret)
                       false
                       (f x))) true preds))))

(defn my-map [f & a-seq]
  (if (coll? (first a-seq))
    (reverse (reduce (fn [acc, v]
                       (if (coll? v)
                         (cons (my-map f v) acc)
                         (cons (f v) acc))) '() (first a-seq)))
    (f (first a-seq))))
