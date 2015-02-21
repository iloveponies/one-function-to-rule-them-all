(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
      (reduce (fn [a b] (str a " " b))
              a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [a b] (conj a x b))
            [(first a-seq)]
            (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [count _] (inc count))
          0
          a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [as b] (cons b as))
          '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq) []
      (let [first-elem (first a-seq)]
        (reduce (fn [min-max cur] [(min cur (get min-max 0))
                                   (max cur (get min-max 1))])
                [first-elem first-elem]
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

(defn pred-and [& preds]
  (fn [x] (reduce (fn [cur pred?] (and cur (pred? x)))
                  true
                  preds)))

(defn my-map
  ([f xs]
   (loop [acc []
          xs' xs]
     (if (empty? xs')
       acc
       (recur (conj acc (f (first xs')))
              (rest xs')))))
  ([f a-set b-set]
   (loop [acc []
          as a-set
          bs b-set]
     (if (or (empty? as) (empty? bs))
       acc
       (recur (conj acc (f (first as) (first bs)))
              (rest as)
              (rest bs)))))
  ([f a-set b-set & more]
   (my-map (fn [xs] (apply f xs))
           (reduce (fn [a b] (my-map conj a b))
                   (my-map vector a-set)
                   (cons b-set more)))))
