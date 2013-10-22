(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
    (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
  (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) ()
  (rest (reduce #(conj %1 x %2) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [a b] (+ a 1)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (conj a b)) () a-seq))

(defn min-max-element [a-seq]
  (let [d (first a-seq)]
  (reduce (fn [[a b] c] (let [x (if (< c a) c a)
                              y (if (> c b) c b)]
                          [x y])) [d d] a-seq)))

(defn insert [sorted-seq n]
  (let [a (filter #(< % n) sorted-seq)
        b (filter #(> % n) sorted-seq)]
    (concat a (conj b n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [fu (fn [s i] (if (contains? s i)
                        (disj s i)
                        (conj s i)))]
    (reduce fu #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (my-count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & m] (reduce * (conj m x y))))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & m] (reduce pred-and p1 (conj m p2))))

(defn my-map
  ([f a-seq] (reduce #(conj %1 (f %2)) [] a-seq))
  ([f a & rst] (if (some empty? (conj rst a))
                  ()
                  (conj (apply my-map f (my-map rest (conj rst a)))
                        (apply f (my-map first (conj rst a)))))))
