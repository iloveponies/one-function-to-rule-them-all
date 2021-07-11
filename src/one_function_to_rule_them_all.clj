(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq) )

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq )
    ))

(defn my-interpose [x a-seq]
  (if
    (empty? a-seq) []
    (cons (first a-seq) (reduce #(conj (conj %1 x) %2) [] (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) [] a-seq))

(defn min-max-element [a-seq]
  (reduce
   (fn [[m1 m2] val] [(min m1 val) (max m2 val)])
   [9999 0]
   a-seq))

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq)
     [n]
   (>= (first sorted-seq) n )
     (cons n sorted-seq)
   :else
     (cons (first sorted-seq) (insert (rest sorted-seq) n))
   ))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [nf (fn [a-set x]
             (if
               (contains? a-set x)
               (disj a-set x)
               (conj a-set x)))
             ]
    (reduce nf #{} a-seq)
    ))

(defn minus
  ([x]
  ( - x))
  ([x y]
  (- x y))
  )

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more))

  )

(defn pred-and
  ([] (fn [x] true))
  ([x] x)
  ([x y] #(and (x %) (y %)))
  ([x y & more] (reduce pred-and (pred-and x y) more))
  )

(defn my-map
  ([f s1 ] (reduce (fn [x y] (conj x (f y))) [] s1))
  ([f s1 & ss]
   (let [seqs (concat [s1] ss)
         maxels (apply min (my-map count seqs))]
     (loop [acc [] loopseqs seqs lim maxels]
       (if (zero? lim)
         acc
         (recur
          (conj acc (apply f (my-map first loopseqs)))
          (my-map rest loopseqs)
          (dec lim)))))))
