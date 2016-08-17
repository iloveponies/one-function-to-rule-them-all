(ns one-function-to-rule-them-all)
(use 'clojure.string)

(defn concat-elements [a-seq]
  (reduce concat () a-seq)
  )

(defn str-cat [a-seq]
  (trimr (reduce #(str %1 %2 " ") "" a-seq))
  )

(defn my-interpose [x a-seq]
  (drop-last (reduce #(concat %1 (vector %2 x)) [] a-seq))
  )

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq)
  )

(defn my-reverse [a-seq]
  (reduce conj () a-seq)
  )

(defn min-max-element [a-seq]
  [ (reduce min a-seq) 
    (reduce max a-seq)
   ]
  )

(defn insert [sorted-seq n]
  (concat 
    (take-while #(> n %) sorted-seq)
    (cons n 
          (drop-while #(> n %) sorted-seq)
          )
    )
  )

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq)
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn parity [a-seq]
  (reduce #(toggle %1 %2) #{} a-seq)
  )

(defn minus 
  ([x] (- x))
  ([x y] (- x y))
  )

(defn count-params 
  ([& x] (count x))
  )

(defn my-* 
  ([] 1)
  ([x] x)
  ([x & y] (reduce * x y))
  )

(defn pred-and 
  ([]  (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p q] (fn [x] (and (p x) (q x))))
;  ([p q & r] (fn [x] (every? (concat (list (p x) (q x)) (map (fn [j] (j x)) r)))))
  )

(defn my-map [f a-seq]
  [:-])
