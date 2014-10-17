(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]

  (reduce concat  a-seq)

  )



(defn str-cat [a-seq]

  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)
    )

  )


(defn my-interpose [x a-seq]

  (reverse (rest (reduce
   (fn [a b] (conj a b x)) '() a-seq
   )))

  )


(defn my-count [a-seq]

  (reduce (fn [cnt elem] (inc cnt)) 0 a-seq)

  )


(defn my-reverse [a-seq]

  (reduce (fn [s a] (conj s a)) '() a-seq)

  )

(defn min-max-element [a-seq]

  (reduce (fn [[x n] el]

            (let [a (if (< el x) el x)
                  b (if (> el n) el n)
                  ]

              [a b]
              )
            ) [99999999 -99999999] a-seq )

  )

(defn insert [sorted-seq n]

  (
   loop [beg [] r sorted-seq]
   (cond
    (empty? r) (conj beg n)
    (>= (first r) n) (concat beg (cons n r) )
    :else
      (recur (conj beg (first r)) (rest r))
     )
   )

  )

(defn insertion-sort [a-seq]

  (reduce insert '() a-seq)

  )



(defn parity [a-seq]


    (into #{} (keys ( filter (fn [[a b]] (odd? b))
      (reduce  (fn [a b]  (assoc a b  (inc (get a b)) ) ) (into {} (map (fn [k] [k 0]) a-seq)) a-seq)
    )))
  )


(defn minus [x]
  :-)

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
