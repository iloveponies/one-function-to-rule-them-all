(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [str1 str2]
              (str str1 " " str2))
            a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [eka toka]
              (conj eka x toka)) [(first a-seq)](rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [counter seq1]
            (inc counter)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b]
            (conj a b)) () a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [ret a]
            (cond
              (< a (first ret))
              [a (second ret)]
              (> a (second ret))
              [(first ret) a]
              :else ret))
          [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (sort (conj sorted-seq n)))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (reduce (fn [mappi x]
            (if (odd? (get (frequencies a-seq) x))
              (conj mappi x)
              mappi))
          #{}
          a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([x & more]
  (inc (count more)))
  ([] 0))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn pred-and ([& more]
   (fn [x]
     (if (empty? more)
       true
       (reduce (fn [bool pred]
                 (and bool (pred x))) true more)))))



(defn my-map [f a-seq]
  [:-])
