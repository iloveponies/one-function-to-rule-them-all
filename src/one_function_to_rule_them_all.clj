(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat "" a-seq)
)

(defn str-cat [a-seq]
  (if
    (empty? a-seq)
    ""
    (reduce #(clojure.string/join [%1 " " %2]) a-seq)
  )
)

(defn my-interpose [x a-seq]
  (rest (reduce #(conj (conj %1 x) %2) [] a-seq))
)

(defn my-count [a-seq]
  (reduce (fn [x _] (inc x)) 0 a-seq)
)

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) "" a-seq)
)

(defn min-max-element [a-seq]
  (reduce
    (fn [[minSoFar maxSoFar] x]
      (let
        [myMin (min x (or minSoFar x))
         myMax (max x (or maxSoFar x))]
        [myMin myMax]
      )
    )
    ""
    a-seq
  )
)

(defn insert [sorted-seq n]
  (concat (take-while #(<= % n) sorted-seq) [n] (drop-while #(<= % n) sorted-seq))
)

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq)
)

(defn parity-helper [a-seq]
  (reduce
    (fn [res [key val]]
      (if 
        (odd? val)
        (conj res key)
        res
      )
    )
    #{}
    (frequencies a-seq)
  )
)

(defn parity [a-seq]
  (set (keys (filter (fn [[_ val]] (odd? val)) (frequencies a-seq))))
)

(defn minus
  ([x] (* x -1))
  ([x y] (- x y))
)

(defn count-params [& more]
  (count more)
)

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce #(* %1 %2) (my-* x y) more))
)

(defn pred-and
  ([] (fn [_] true))
  ([pred] pred)
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & rest]
    (reduce
      #(pred-and %1 %2)
      (pred-and pred1 pred2)
      rest
    )
  )
)

(defn my-map
  ([f & a-seq]
    (loop [a-seq a-seq
           results []]
      (cond (some empty? a-seq)
        results
        :else (let
          [firsts (reduce #(conj %1 (first %2)) [] a-seq)
           rests (reduce #(conj %1 (rest %2)) [] a-seq)
           result (apply f firsts)]
           (recur rests (conj results result))
        )
      )
    )
  )
)
