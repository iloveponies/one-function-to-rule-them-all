(ns one-function-to-rule-them-all)


(defn concat-elements [a-seq]
  (reduce concat a-seq)
)

(defn str-cat-sp [str1 str2]
  (if (and (not (empty? str1)) (not (empty? str2)))
      (str str1 " " str2)
      (str str1 str2)
  )
)

(defn str-cat [a-seq]
  (reduce str-cat-sp "" a-seq)
)

(defn my-interpose [x a-seq]
  (let [helper (fn [e1 e2] (if (not (empty? e1))
                               (concat e1 (list x e2))
                               (list e2)
                            )
               )
       ]
      (reduce helper [] a-seq)
  )
)

(defn my-count [a-seq]
  (let [counter  (fn [count e] (inc count))]
    (reduce counter 0 a-seq)
  )
)


(defn my-reverse [a-seq]
   (let [rev (fn [e1 e2] (cons e2 e1))]
      (reduce rev '() a-seq)
   )
)

(defn min-max-element [a-seq]
   (let [counth (fn [counter e]
                   (cond
                     (< e (counter 0))
                       [e (counter 1 )]
                     (> e (counter 1 ))
                       [(counter 0) e]
                     :else
                       counter
                    )
                 )
        ]
       (reduce counth [(first a-seq) (first a-seq)] (rest a-seq))
    )
)

(defn insert [sorted-seq n]
   (if (or (empty? sorted-seq) (< n (first sorted-seq)))
       (cons n sorted-seq)
       (cons (first sorted-seq) (insert (rest sorted-seq) n))
   )
)

(defn insertion-sort [a-seq]
   (reduce insert [] a-seq)
)

(defn parity [a-seq]
   (let [toggle (fn [odds e] (if (odds e)
                                 (disj odds e)
                                 (conj odds e)
                              )
                )
        ]
     (reduce toggle #{} a-seq)
   )
)

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y))
)

(defn count-params [& more]
  (let [counter  (fn [count p] (inc count))]
    (reduce counter 0 more)
  )
)

(defn my-* [& more]
  (let [multiplier (fn [acum p] (* acum p)) ]
     (reduce multiplier 1 more)
  )
)



(defn pred-and
  ([] (fn [x] x))
  ([ & more]
      (fn [x]
         (let [preds (fn [anded pred] (and anded (pred x)))]
             (reduce preds number? more)
         )
      )
  )
)

(defn my-map
   ([f & more]
     (if (every? empty? more)
         '()
         (cons (apply f (remove nil? (map first more))) (apply my-map (cons f (map rest more))))
     )
   )
)

