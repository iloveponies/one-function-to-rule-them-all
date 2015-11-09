(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce 
    (fn [acc el] (concat acc el))
    [] 
    a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
  (reduce
    (fn [acc el] (str acc " " el))
    a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) a-seq
    (reduce 
      (fn [acc el] (conj acc x el))
      [(first a-seq)]
      (rest a-seq))))

(defn my-count [a-seq]
  (reduce
    (fn [acc el] (inc acc))
    0
    a-seq))

(defn my-reverse [a-seq]
  (reduce
    (fn [acc el] (cons el acc))
    '()
    a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq) []
  [
   (reduce min a-seq)
   (reduce max a-seq)
  ]))

(defn insert [sorted-seq n]
  (let [reduce-helper 
        (fn [sorted-seq n acc]
          (let [_1st (first sorted-seq)]
            (if (not (nil? _1st))
              (if (<= _1st n)
                (recur (rest sorted-seq) n (conj acc _1st))
                (into (conj acc n) sorted-seq))
              (conj acc n))))]
  (reduce-helper sorted-seq n [])))

(defn insertion-sort [a-seq]
  (if (empty? a-seq) a-seq
    (reduce
      (fn [acc el] (insert acc el))
      []
      a-seq)))

(defn parity [a-seq]
  (let [freq (frequencies a-seq)]
    (reduce
      (fn [acc [k v]]
        (if (odd? v)
          (conj acc k)
          acc))
      #{}
      freq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-* [& x]
  (let [cnt (count x)]
    (cond 
      (= cnt 0) 1
      (= cnt 1) (first x)
      :else (apply * x))))

(defn pred-and [& x]
  (let [ct (count x)]
    (fn [arg] 
      (cond
        (= ct 0) true
        :else (reduce
                (fn [acc predicate] (and acc (predicate arg)))
                true
                x)))))

(defn my-map [f a-seq & more-seq]
  )

