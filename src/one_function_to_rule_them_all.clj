(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq)
  )

(defn str-cat [a-seq]
  (let [strFn (fn [y x] (str y " " x))]
    (cond
      (empty? a-seq) ""
      :else (reduce strFn a-seq)
      )
    )
  )

(defn my-interpose [x a-seq]
  (let [my (fn [seq1 next]
             (cond
               (empty? seq1) [next]
               :else (conj seq1 x next)))
        v (vec a-seq)]
    (cond
      (empty? v) v
      (== (count v) 1) v
      :else (reduce my [] v)
      )
    ))

(defn my-count [a-seq]
  (let [helper (fn [acc b] (inc acc))]
    (cond
      (empty? a-seq) 0
      :else (reduce helper 0 a-seq)
      )
    )
  )

(defn my-reverse [a-seq]
  (let [helper (fn [acc b] (cons b acc))]
    (cond
      (empty? a-seq) a-seq
      (== 1 (count a-seq)) a-seq
      :else (reduce helper [] a-seq)
      )
    )
  )

(defn min-max-element [a-seq]
  (let [helper (fn [acc b]
                 (cond (empty? acc) [b b]
                       :else (let [mi (first acc)
                                   ma (second acc)
                                   nmi (if (< b mi) b mi)
                                   nma (if (> b ma) b ma)]
                               [nmi nma])
                       )
                 )]
    (cond
      (empty? a-seq) []
      (== 1 (count a-seq)) [(first a-seq) (first a-seq)]
      :else (reduce helper [] a-seq)
      )
    )
  )

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (cons n sorted-seq)
    (if (< (first sorted-seq) n)
      (cons (first sorted-seq) (insert (rest sorted-seq) n))
      (cons n sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more))
  )


(defn pred-and
  ([] (fn [n] true))
  ([x] (fn [n] (x n)))
  ([x y] (fn [n] (and (x n) (y n))))
  ([x y & more] (fn [n] (reduce
                          (fn [b1 b2] (and b1 (b2 n)))
                          (and (x n) (y n))
                          more)))
  )


;Write the function my-map that works just like standard map.
;It takes one or more sequences and a function f that takes as many parameters as there are sequences.
(defn my-map
  ([f a-seq] (map f a-seq))
  ([f coll & more]
   (let [colls (cons coll more)]
     (if (some empty? colls)
       '()
       (cons (apply f (my-map first colls))
             (apply my-map f (my-map rest colls)))))))
