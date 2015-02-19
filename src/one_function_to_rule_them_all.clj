(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b))
            (first a-seq)
            (rest a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [a b] (conj a x b))
            [(first a-seq)]
            (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [n _] (inc n))
          0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [as b] (cons b as))
          '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (let [a (first a-seq)]
      (reduce (fn [as b] [(if (< b (get as 0)) b (get as 0))
                          (if (> b (get as 1)) b (get as 1))])
              [a a]
              (rest a-seq)))))

(defn insert [sorted-seq n]
  (cond (empty? sorted-seq)      (list n)
        (< n (first sorted-seq)) (cons n sorted-seq)
        :else                    (cons (first sorted-seq)
                                       (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce (fn [as a] (if (contains? as a)
                       (disj as a)
                       (conj as a)))
          #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (reduce (fn [acc _] (inc acc))
          0 more))

(defn my-* [& more]
  (reduce * 1 more))

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
