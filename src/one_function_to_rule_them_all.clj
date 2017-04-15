(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) "" (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) () (reduce (fn [a b] (conj a x b)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (let [c (fn [c x]
            (inc c))]
    (reduce c 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev (fn [B a]
              (conj B a))]
    (reduce rev () a-seq)))

(defn min-max-element [a-seq]
  (vector (apply min a-seq) (apply max a-seq)))

(defn insert [sorted-seq n]
  (let [f (fn [x] (< x n))
        head (take-while f sorted-seq)
        tail (drop-while f sorted-seq)]
    (concat head [n] tail)))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (let [f (fn [a-set x]
            (if (contains? a-set x)
              (disj a-set x)
              (conj a-set x)))]
    (reduce f #{} a-seq)))

(defn minus ([x] (* -1 x))
            ([x y] (- x y)))


(defn count-params [& more] (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))


(defn pred-and
  ([] (fn [x] true))
  ([px] (fn [x] (px x)))
  ([px py] (fn [x] (and (px x) (py x))))
  ([px py & more]
   (reduce pred-and (pred-and px py) more)))

(defn my-map
  [-])
