(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
   (reduce concat [] a-seq)
)

(defn str-cat [a-seq]
   (if (empty? a-seq)
       ""
      (reduce str "" (interpose " " a-seq))
   )
)

(defn my-interpose [x a-seq]
   (if (empty? a-seq)
      '()
      (reverse (reduce (fn [initial elem] 
                 (if (empty? initial)	
                   (conj initial elem)
                   (conj (conj initial x) elem)
                 )
              ) 
       '() a-seq))
   )
)

(defn my-count [a-seq]
   (reduce (fn [initial elem] (+ initial 1)) 0 a-seq)
)

(defn my-reverse [a-seq]
   (reduce conj '() a-seq)
)

(defn min-max-element [a-seq]
  (reduce 
    (fn [init elem] 
               (conj [] (min (get init 0) elem) (max (get init 1) elem)))
    (conj [] (first a-seq) (first a-seq))
    (rest a-seq)
  )
)

(defn insert [sorted-seq n]
 (seq(loop [seq1 sorted-seq
          seq2 []]
       (cond
          (empty? seq1) (conj seq2 n)
          (<= n (first seq1)) (concat (conj seq2 n) seq1)
          :else (recur (rest sorted-seq) (conj seq2 (first seq1)))
       )
   ))
)

(defn insertion-sort [a-seq]
    (reduce insert [] a-seq)
)

(defn parity [a-seq]
   (loop [seq1 a-seq
          set1 (set [])]
     (if (empty? seq1)
         set1
         (recur (rest seq1) 
                (if (contains? set1 (first seq1))
                   (disj set1 (first seq1))
                   (conj set1 (first seq1))
                )
         )
     )
   )
)

(defn minus
   ([x] (- x))
   ([x y] (- x y))
)

(defn count-params 
  ([& more]
     (count more)
  )
)

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more))
)

(defn pred-and
  ([] (fn [elem] true))
  ([f] (fn [elem] (f elem)))
  ([f1 f2] (fn [elem] (and (f1 elem) (f2 elem))))
  ([f1 f2 & more] (reduce pred-and (pred-and f1 f2) more))
)

(defn my-map [f a-seq]
  [:-])