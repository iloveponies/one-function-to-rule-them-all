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


(defn minus

  ([a] (- a))
  ([a b] (- a b))

  )


(defn count-params

  ([& more] (count more))

  )


(defn my-*

  ([] 1)
  ([x y & more]
   (* (* x y ) (reduce * more))
   )

  )

(my-*)           ;=> 1
(my-* 4 3)       ;=> 12
(my-* 1 2 3 4 5) ;=> 120

(defn pred-and

  ([] (fn [val] true))
  ([& more] (fn [val] (reduce (fn [res q?] (if (false? res) res (q? val))) true more) ))

  )

(defn my-map [f a-seq]
  [:-])
