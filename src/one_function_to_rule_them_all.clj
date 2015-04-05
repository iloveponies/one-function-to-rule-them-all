(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
(if (empty? a-seq)
  ()
  (seq (reduce concat a-seq))
  )
  )


(defn str-cat [a-seq]
(if (empty? a-seq)
  ""
 (reduce #(str %1 " " %2) a-seq)
  )
  )

(defn my-interpose [x a-seq]
(drop 1 (interleave (repeat x) a-seq))
  )

(defn my-count [a-seq]
(if (empty? a-seq)
  0
    (reduce + (map (fn [x] 1) a-seq))
  )
  )

(defn my-reverse [a-seq]
 (if (empty? a-seq)
   ()
   (reverse a-seq)

   )

  )

(defn min-max-element [a-seq]

  [(reduce min a-seq) (reduce max a-seq)]
  )


(defn insert [sorted-seq n]

  (sort (cons n sorted-seq))
  )

(defn insertion-sort [a-seq]
(reduce insert [] a-seq)
  )

(defn parity [a-seq]
(let [freqs (fn [freq x]
(assoc freq x
(if (contains? freq x)
(inc (freq x))
1)))]
(set (keys (filter #(odd? (val %)) (reduce freqs {} a-seq))))))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y))
  )

(defn count-params [& more]
 (count more)
)

(defn my-*
([] 1)
([x] x)
([x y] (* x y))
([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
([] (fn [x] true))
([x] #(x %))
([x & more] #(reduce (fn [a b] (and a (b %))) (x %) more)))
(defn my-map [f & a-seq]
(cond
(some empty? a-seq)
'()
:else
(cons (apply f (map first a-seq))
(apply (partial my-map f) (map rest a-seq)))))
