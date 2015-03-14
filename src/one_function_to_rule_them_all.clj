(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq))
  )

(defn my-interpose [x a-seq]
  (if (empty? a-seq) a-seq
    (reduce #(conj %1 x (first %2)) (map #(vector %) a-seq))) )


(defn my-count [a-seq]
  (reduce (fn [a _] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (if (empty? a-seq) a-seq
    (reduce #(conj %1 (first %2)) (map #(list %) a-seq))))


(defn min-max-element [a-seq]
  (let [minmax-f (fn [[acu-min acu-max] next-element]
                   (cond
                    (< next-element acu-min) [next-element acu-max]
                    (> next-element acu-max) [acu-min next-element]
                    :else [acu-min acu-max]
                    ))]

    (reduce minmax-f (cons (repeat 2 (first a-seq)) (rest a-seq)))))

(defn insert [sorted-seq n]
  (let [helper (defn helper [seq-head seq-rest]
                 (if (empty? seq-rest)
                   (conj seq-head n)
                   (let [first-of-rest (first seq-rest)]
                     (if (> n first-of-rest)
                       (helper (conj seq-head first-of-rest) (rest seq-rest))
                       (concat (conj seq-head n) seq-rest)))))]
    (helper [] sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))


(defn count-params [ & more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (my-* x y) more)))

(defn pred-and
  ([] (fn [a] true))
  ([x] (fn [a] (x a)))
  ([x & more] (fn [a] (reduce #(and %1 (%2 a)) (x a) more))))

(filter (pred-and) [1 0 -2])                    ;=> (1 0 -2)
(filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> (1 7)
(filter (pred-and number? integer? pos? even?)
        [1 0 -2 :a 7 "a" 2])

(defn seq-of-seqs-resuce [ & coll ]
  (reduce #(conj %1 (first %2)) [] coll))

(defn interleave-and-split
  ([c] c)
  ([c & coll]
  (partition (count (first coll)) (apply interleave (cons c coll)))))

(interleave-and-split [1 2 3] [1 2 3] [1 2 3])
(interleave-and-split [1 2 3 4])

(defn my-map [f & a-seqs]
  (reduce #(conj %1 (apply f %2)) [] (interleave-and-split a-seqs)))

;;(my-map inc [1 2 3 4])                  ;=> (2 3 4 5)
;;(my-map + [1 1 1] [1 1 1] [1 1 1])      ;=> (3 3 3)
;;(my-map vector [1 2 3] [1 2 3] [1 2 3]) ;=> ((1 1 1) (2 2 2) (3 3 3))
