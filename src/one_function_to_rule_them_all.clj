(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq)
  )


(defn str-cat [a-seq]
  (let [concatTwo (fn [a b] (str a " " b))]
  (cond
   (empty? a-seq) ""
   :else          (reduce concatTwo a-seq)
  )))


(defn my-interpose [x a-seq]
  (let [addBetween (fn [a1 b1]
                       (conj a1 x b1))]
  (cond
   (empty? a-seq)       ()
   (== (count a-seq) 1) (into () a-seq)
   :else                (rest(reduce addBetween [] a-seq))
  )))


(defn my-count [a-seq]
  (let [addOne (fn [acc _]
                (inc acc))]
  (reduce addOne 0 a-seq)
  ))


(defn my-reverse [a-seq]
  (let [constructNew (fn [a b]
                       (conj a b))]
   (reduce constructNew () a-seq)
   ))


(defn min-max-element [a-seq]
  (let [findMin (fn [a b]
                 (min a b))
        findMax (fn [a b]
                  (max a b))]
    (conj (vector (reduce findMin a-seq)) (reduce findMax a-seq))
    ))



(defn insert [sorted-seq n]
   (sort (conj sorted-seq n))
  )


(defn insertion-sort [a-seq]
  (reduce insert () a-seq)
  )

(defn countOccurances [a-seq elem]
  (count (filter #{elem} a-seq))
  )

(defn parity [a-seq]
  (let [setted (set a-seq)
        formNew (fn [setted a-seq original]
                  (cond
                   (empty? setted)                                       original
                   (== (mod (countOccurances a-seq (first setted)) 2) 1) (recur (rest setted) a-seq original)
                   :else                                                 (recur (rest setted) a-seq (disj original (first setted)))
                  ))]
    (formNew setted a-seq setted)

  ))


(defn minus
  ([x]   (* x -1))
  ([x y] (- x y))
  )

(defn count-params [& more]
  (let [looper (fn [x y]
                 (inc x)
                 )]
  (reduce looper 0 more)
  ))


(defn my-*
  ([] 1)
  ([x] x)
  ([x & more] (reduce * x (my-* more)))
  )


(defn pred-and
  ([] (fn [x] ()))
  ;([p] (fn [x] (p x)))
  ;([p q] (fn [x] (and (p x) (q x))))
  ([& more] (fn [x] (reduce
                     (fn [a p] (and a (p x)))
                     true more)))
  )

(filter (pred-and pos?) [1 0 -2])                    ;=> (1 0 -2)
(filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> (1 7)
(filter (pred-and number? integer? pos? even?)
        [1 0 -2 :a 7 "a" 2])                    ;=> (0 2)

(defn my-map [f a-seq]
  [:-])
