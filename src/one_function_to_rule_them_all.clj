(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce
    concat
    ()
    a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce
      (fn [word1 word2] (str word1 " " word2))
      a-seq)))


(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce
      (fn [item1 item2]
        (conj item1 x item2))
      (conj '() (first (reverse a-seq)))
      (rest (reverse a-seq)))))

(defn my-count [a-seq]
  (reduce
    (fn [counter elem] (inc counter))
    0
    a-seq))

(defn my-reverse [a-seq]
  (reduce
    #(conj %1 %2)
    '()
    a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (reduce
       (fn [min-max-vec number]
         (if (< number (first min-max-vec))
           [number (second min-max-vec)]
           (if (> number (second min-max-vec))
             [(first min-max-vec) number]
             min-max-vec)))
       [(first a-seq) (first a-seq)]
       a-seq)))

(defn insert [sorted-seq n]
  (concat
    (take-while
      (fn [x] (< x n))
      sorted-seq)
    (conj () n)
    (drop-while
      (fn [x] (< x n))
      sorted-seq)))

(defn insertion-sort [a-seq]
  (if (empty? a-seq)
    '()
    (reduce
      (fn [sorted-seq elem]
        (insert sorted-seq elem))
      []
      a-seq)))

(defn parity [a-seq]
  (if (empty? a-seq)
    #{}
    (reduce
      (fn [the-set elem]
        (if (contains? the-set elem)
          (disj the-set elem)
          (conj the-set elem)))
      #{}
      a-seq)))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x & more] (+ 1 (count more))))


(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])