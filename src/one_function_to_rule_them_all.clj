(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc s] (apply str (str acc " " s))) (first a-seq) (rest a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (reduce (fn [acc s] (conj (conj acc x) s)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc s] (+ acc 1)) 0 a-seq))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    ()
    (reduce (fn [acc s] (cons s acc)) [(first a-seq)] (rest a-seq))))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [minMax s] (if (< s (first minMax))
                             [s (second minMax)]
                             (if (> s (second minMax))
                               [(first minMax) s]
                               minMax)))
            [(first a-seq) (first a-seq)] (rest a-seq))))


(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
    (if (> n (first sorted-seq)) 
      (cons (first sorted-seq) (insert (rest sorted-seq) n))
      (cons n sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce (fn [sorted-seq n] (insert sorted-seq n)) [(first a-seq)] (rest a-seq)))

(defn parity [a-seq]
  (if (empty? a-seq)
    #{}
    (reduce (fn [a-set val]
              (if (contains? a-set val)
                (disj a-set val)
                (conj a-set val))) #{(first a-seq)} (rest a-seq))))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn empty-func 
  ([] 0)
  ([x & more]
  (if (nil? x)
    "empty"
    "not empty")))

(defn count-params
  ([] 0)
  ([x & more] (if (nil? x)
                0
                (+ (count more) 1))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (* (reduce (fn [prod val] (* prod val)) 1 more) x y)))


(defn pred-and
  ([] (fn [x] (or (pos? x) (neg? x) (complement (nil? x)))))
  ([pred] pred)
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more] (fn [x] (and 
                                  (pred1 x) 
                                  (pred2 x) 
                                  (reduce (fn [acc pred] (and acc (pred x)))
                                          true more)))))

(defn my-map [f a-seq]
  [:-])