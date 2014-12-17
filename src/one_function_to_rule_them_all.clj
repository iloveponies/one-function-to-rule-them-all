(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (if (empty? a-seq)
    '()
    (reduce concat (empty seq) a-seq)))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [ipose (fn [acc elem] (str acc " " elem))]
      (reduce ipose a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (let [v []
          ipose (fn [acc elem] (if (empty? acc)
                                 (concat acc (list elem))
                                 (concat acc (list x) (list elem))))]
      (reduce ipose v a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [length elem] (inc length))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [r-seq []
        rev (fn [acc elem] (concat (list elem) acc))]
    (reduce rev r-seq a-seq)))

(defn min-max-element [a-seq]
  (concat (list (reduce min a-seq)) (list (reduce max a-seq))))

(defn insert [sorted-seq n]
  (concat (take-while (fn [x] (< x n)) sorted-seq)
          (list n)
          (drop-while (fn [x] (< x n)) sorted-seq)))

(defn insertion-sort [a-seq]
  (let [r-vec []]
    (reduce insert r-vec a-seq)))

(defn parity [a-seq]
  (let [r-seq #{}
        par (fn [acc elem] (if (contains? acc elem)
                         (disj acc elem)
                         (conj acc elem)))]
    (reduce par r-seq a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (let [counter (fn [count y] (inc count))]
    (reduce counter 0 x)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (fn [x] (reduce (fn [acc pred] (and acc (pred x))) (and (p1 x) (p2 x)) more))))

(defn my-map
  ([f a-seq] (reduce (fn [x y] (concat x (list (f y)))) [] a-seq))
  ([f a-seq & more]
   (loop [r-seq []
          n 0]
     (if (= n (count a-seq))
       r-seq
       (recur (concat r-seq (list (apply f (reduce (fn [acc elem] (concat acc (list (get elem n))))
                                                   []
                                                   (cons a-seq more)))))
              (inc n))))))
