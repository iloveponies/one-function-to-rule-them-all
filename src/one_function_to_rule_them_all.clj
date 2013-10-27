(ns one-function-to-rule-them-all)

(defn concat-elements
  [a-seq]
  (reduce concat a-seq))

(defn str-cat
  [a-seq]
  (if (empty? a-seq)
    ""
    (.trim (reduce (fn [acc x]
                     (str acc " " x))
                   a-seq))))

(defn my-interpose
  [x a-seq]
  (rest
    (reduce (fn [acc y]
              (conj acc x y))
            []
            a-seq)))

(defn my-count
  [a-seq]
  (reduce (fn [acc _]
            (+ acc 1))
          0
          a-seq))

(defn my-reverse
  [a-seq]
  (reduce #(cons %2 %1)
          ()
          a-seq))

(defn min-max-element
  [a-seq]
  (reduce (fn [[mini maxi] x]
            (if (and mini maxi)
              [(min mini x) (max maxi x)]
              [x x]))
          []
          a-seq))

(defn insert
  [sorted-seq n]
  (concat (take-while #(<= % n) sorted-seq)
          [n]
          (drop-while #(< % n) sorted-seq)))

(defn insertion-sort
  [a-seq]
  (reduce insert [] a-seq))

(defn- toggle
  [a-set elem]
  (if (a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity
  [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x]
   (- x))
  ([x y]
   (- x y)))

(defn count-params
  [& xs]
  (count xs))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (constantly true))
  ([p] p)
  ([p1 p2 & more] (fn [x]
                    (reduce (fn [acc pred]
                              (and acc (pred x)))
                            (and (p1 x) (p2 x))
                            more))))

(defn my-map
  ([f a-seq]
   (reduce (fn [acc x] (conj acc (f x))) [] a-seq))
  ([f coll & more]
   (loop [i 0 acc []]
     (if (some (partial = i) (my-map count (cons coll more)))
       acc
       (recur (inc i) (conj acc (apply f (my-map #(get % i) (cons coll more)))))))))

