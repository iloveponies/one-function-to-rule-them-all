(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (zero? (count a-seq))
    ""
    (reduce (fn [v w]
              (str v " " w))
            a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [v w]
              (conj (conj v x) w))
            (conj [] (first a-seq))
            (rest a-seq))))

(defn my-count [a-seq]
  (let [f (fn [x u]
            (+ x 1))]
    (reduce f 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
    (reduce (fn [[pmin pmax] x]
              (vector (min pmin x)
                      (max pmax x)))
            (vector (first a-seq) (first a-seq))
            (rest a-seq)))

(defn insert [sorted-seq n]
  (let [i (loop [j 0]
            (if (or (= j (count sorted-seq))
                    (< n (nth sorted-seq j)))
              j
              (recur (inc j))))]
    (concat (take i sorted-seq) (conj (drop i sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [a-set elem]
            (if (contains? a-set elem)
              (disj a-set elem)
              (conj a-set elem)))
          #{}
          a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (reduce (fn [sum x] (+ sum 1)) 0 more))

(defn my-* [& more]
  (reduce (fn [prod x] (* prod x)) 1 more))

(defn pred-and [& more]
  (reduce (fn [sum pred] (fn [y] (and (sum y) (pred y))))
          (fn [y] true)
          more))

(defn my-map [f & more]
  (loop [i 0
         result []]
    (if (= (count (first more)) i)
      result
      (recur (inc i)
             (conj result
                   (apply f (for [arr more]
                              (nth arr i))))))))
