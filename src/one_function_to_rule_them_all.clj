(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
  (let [help-str (fn [x y] (str x " " y) )]
    (reduce help-str a-seq)
  ))
)

(defn my-interpose [x a-seq]
  (if (empty? a-seq) ()
  (let [helper (fn [pre post] (conj pre x post) )]
    (reduce helper [(first a-seq)] (rest a-seq))
  ))
  )

(defn my-count [a-seq]
  (let [helper (fn [count elem] (inc count))   ]
    (reduce helper 0 a-seq)
  ))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [helper (fn [x y] (let [min (first x)
                               max (last x)]
                                  [(if (< y min) y min)
                                   (if (> y max) y max)]
                               ))]
    (reduce helper [(first a-seq) (first a-seq)] (rest a-seq))
    )
  )

(defn insert-help [to-be-sorted sorted n]
  (cond
    (empty? to-be-sorted) (conj sorted n)
    (> (first to-be-sorted) n) (concat (conj sorted n) to-be-sorted)
    :else (recur (rest to-be-sorted) (conj sorted (first to-be-sorted)) n)
    ))

(defn insert [sorted-seq n]
    (insert-help sorted-seq [] n)
  )

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
  )
)

(defn parity [a-seq]
  (reduce toggle #{} a-seq)
  )

(defn minus
 ([x] (- x))
 ([x y] (- x y)))

(defn count-params
  ([]  0)
  ([x] 1)
  ([x & more] (reduce (fn [x y] (inc x)) 1 more ))
)

(defn my-*
  ([]  1)
  ([x] x)
  ([x & more] (reduce * x more))
)

(defn pred-and
  ([] (fn [x] true ) )
  ([x] x )
  ([head & more] (fn [x] (reduce (fn [val pred] (and val (pred x)) ) (head x) more )))
)

(defn my-map [f a-seq]
  [:-])
