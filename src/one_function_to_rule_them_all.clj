(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    (str "")
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
    (if (empty? a-seq)
      ()
      (reduce (fn [a b] (if (empty? a)
                          (conj a b)
                          (conj (conj a x) b)))
                [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (concat [b] a)) [] a-seq ))

(defn min-max-element [a-seq]
  (reduce (fn [a b]
            (if (empty? a)
              (vector b b)
              (vector (min (int (first a)) b) (max (int (last a)) b))))
          [] a-seq))

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) (vector n)
    (< n (int (first sorted-seq))) (concat [n] sorted-seq)
    :else (concat (vector (first sorted-seq)) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [a b]
           (if (contains? a b)
             (disj a b)
             (conj a b)))
         #{} a-seq))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (reduce (fn [a b] (inc a)) 0 more))

(defn my-*
      ([] 1)
      ([x] x)
      ([x y] (* x y))
      ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p1] (fn [x] (p1 x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map [f & more]
  "fail")
