(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce (fn [acc a] (concat acc a)) () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (apply str (reduce (fn [acc a] (concat acc (conj [" "] a))) [(first a-seq)] (rest a-seq)))))

(defn my-interpose [x a-seq]
    ( if (empty? a-seq)
       ()
      (seq (reduce (fn [acc a] (concat acc (conj [x] a))) [(first a-seq)] (rest a-seq)))
    )
  )

(defn my-count [a-seq]
  (reduce (fn [counter a] (inc counter)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc a] (cons a acc )) [] a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [[mn mx] a] [(min mn a) (max mx a)]) [(first a-seq) (first a-seq)] a-seq)
    )
  )


(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (cons n sorted-seq)
    (loop [acc [] b-seq  sorted-seq]
      (if (empty? b-seq)
        (seq ( conj acc n))
        (if (> n (first b-seq))
          (recur (conj acc (first b-seq)) (rest b-seq))
          (concat (conj acc n) b-seq)
        )
      )
    )
  )
)

(defn insertion-sort [a-seq]
  (reduce (fn [acc a] (insert acc a)) [] a-seq))

(defn parity [a-seq]
    (reduce (fn [parity-seq a] (if (contains? parity-seq a) (disj parity-seq a) (conj parity-seq a))) #{} a-seq)
)

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [ & x ]
  (reduce (fn [counter a] (inc counter) ) 0 x)
  )

(defn my-* [& x]
  (reduce (fn [prod a] (* prod a)) 1 x))

(defn pred-and [& pred]

  (fn [x]
    (reduce (fn [result a] (and result (a x)  )) true pred)

    ))

(defn my-map
  ([f a-seq] (reduce (fn [result a] (conj result (f a))) [] a-seq))
  ;([f a-seq1 a-seq2] (reduce (fn [result [a b]] (conj result (apply f [a b]))) (apply f (map first  [a-seq1 a-seq2]))) [(rest a-seq1) ( rest a-seq2)] )
  ([f a-seq1 a-seq2] (map #(apply f %) ( map list a-seq1 a-seq2)))
  ;([f a-seq1 a-seq2 & seq] (map #(apply f %) (apply map list (reduce (fn [result a-seq] (conj  result a-seq))  [a-seq1 a-seq2] seq))))
  ([f a-seq1 a-seq2 & x-seq] (map #(apply f %) (apply map list (conj  x-seq a-seq2 a-seq1)))) ;got this idea from the map code

  )
