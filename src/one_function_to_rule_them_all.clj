(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    (str "")
    (reduce (fn [result current] (str result " " current)) a-seq)))

(defn my-interpose [x a-seq]
  (drop-last (reduce (fn [result current] (conj result current x)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [result current] (inc result)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [result current] (cons current result)) [] a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [result current]
            (if (< current (get result 0))
              (assoc result 0 current)
              (if (> current (get result 1))
                (assoc result 1 current)
                 result)))
           [(first a-seq) (first a-seq)]
           a-seq))

(defn insert [sorted-seq n]
   (sort (conj sorted-seq n)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [result current]
            (if (contains? result current)
              (disj result current)
              (conj result current)))
           #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params ([& more]
  (count more)))

(defn my-*
  ([& more] (reduce * more)))

(defn pred-and
  ([& more] (fn [x]
              (reduce (fn [result current] (if (= false result)
                                                                false
                                                                (current x)))
                true more))))

(defn my-map
  ([f a-seq]
    (reduce (fn [result current] (conj result (f current))) [] a-seq))
  ([f a-seq b-seq]
    (loop [seq1 a-seq
           seq2 b-seq
           comb []]
      (if (or (empty? seq1) (empty? seq2))
        comb
        (if (vector? (first seq1))
          (recur (rest seq1) (rest seq2) (conj comb (flatten (f (first seq1) (first seq2)))))
          (recur (rest seq1) (rest seq2) (conj comb (f (first seq1) (first seq2))))))))
  ([f a-seq b-seq & more]
   (reduce (fn [result current] (my-map f result current)) (my-map f a-seq b-seq) more)))
