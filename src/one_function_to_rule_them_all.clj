(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce #(concat %1 [x %2]) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [cnt elem] (inc cnt)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (let [a (take-while #(< % n) sorted-seq)
        b (drop-while #(< % n) sorted-seq)]
    (reduce concat [a [n] b])))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce #(if (get %1 %2)
             (disj %1 %2)
             (conj %1 %2)) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([x] x)
  ([x y] #(and (x %) (y %)))
  ([x y & more] (fn [z] (reduce #(and %1 (%2 z))
                                true
                                (concat [x y] more)))))

(defn my-map
  ([f a-seq]
   (for [x a-seq]
     (f x)))
  ([f seq1 & seqr]
   (let [seqn (cons seq1 seqr)]
     (for [i (range (apply min (my-map count seqn)))
           :let [params (my-map #(nth % i) seqn)]]
       (apply f params)))))

