(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [memo, s] (str memo " " s)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [memo, elem] (conj memo x elem))
            [(first a-seq)]
            (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [memo, _] (inc memo)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [memo, elem] (cons elem memo)) [] a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (let [pred (fn [x] (< x n))
        head (take-while pred sorted-seq)
        tail (drop-while pred sorted-seq)]
    (concat head (cons n tail))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-*
  ([] 1)
  ([x & more] (* x (reduce my-* more))))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p & more] (fn [x]
                (reduce
                 (fn [memo pred] (if memo (pred x) memo)) (p x)
                 more))))

(defn my-map
    ([f a-seq] (reduce (fn [memo, x] (conj memo (f x))) [] a-seq))
    ([f a-seq & more]
     (let [firsts (my-map first (cons a-seq more))
           rests (my-map rest (cons a-seq more))]
       (when (every? identity firsts)
         (cons (apply f firsts) (apply (partial my-map f) rests))))))

