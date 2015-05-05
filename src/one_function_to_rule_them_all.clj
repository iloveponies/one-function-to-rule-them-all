(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
 (if (empty? a-seq)
   ""
   (reduce (fn [a b] (str a " " b)) a-seq )))
 
(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reverse (drop 1 (reduce (fn [a b] (conj a b x) ) '() a-seq))))
  )

(defn my-count [a-seq]
  (if (empty? a-seq)
    0
    (reduce (fn [a b] (inc a)) 0 a-seq)))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [a b] (conj a b)) '() a-seq)))

(defn min-max-element [a-seq]
  (let [helper (fn [init elem] 
                (if (empty? init)
                  [elem elem]
                  [(first (sort (cons elem init))) (last (sort (cons elem init)))]
                  ))]
    (reduce helper '() a-seq)))

(defn insert [sorted-seq n]
  (loop [sqc sorted-seq
         current []]
    (cond
      (empty? sqc) (concat current (list n))
      (<= (first sqc) n) (recur (rest sqc) (conj current (first sqc)))
      :else (concat current (list n) sqc))))

(defn insertion-sort [a-seq]
  (if (empty? a-seq)
    '()
    (reduce insert '() a-seq)))

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

(defn count-params
  ([] 0)
  ([x] 1)
  ([x y] 2)
  ([x y & more] (reduce (fn [x y] (inc x)) 2 more)))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and 
  [& more] (fn [x] 
             (loop [prev true
                    predicates more]
               (cond
                 (empty? predicates) prev
                 ((first predicates) x) (recur true (rest predicates))
                 :else false))))

(defn my-map 
  ([func coll] 
   (if (empty? coll)
     '()
     (cons (func (first coll)) (my-map func (rest coll))))))