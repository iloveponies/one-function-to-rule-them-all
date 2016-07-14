(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq)
  )

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq))
  )

(defn my-interpose [x a-seq]
  (reverse (rest (reverse (reduce (fn [newseq elem] (concat newseq [elem] [x])) '() a-seq))))
  )

(defn my-count [a-seq]
  (let [counter (fn [yht x]
                    (inc yht))]
    (reduce counter 0 a-seq))
  )

(defn my-reverse [a-seq]
  (let [reversi (fn [uus x]
                  (cons x uus))]
    (reduce reversi '() a-seq))
  )

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)]
  )

(defn insert [sorted-seq n]
  (let [helper (fn [newseq n sorted-seq]
                 (cond
                   (empty? sorted-seq) (concat newseq [n])
                   (< n (first sorted-seq)) (concat newseq [n] sorted-seq)
                   :else (recur (concat newseq [(first sorted-seq)]) n (rest sorted-seq))
                   ))]
    (helper '() n sorted-seq))
  )

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle '#{} a-seq)
    )
  )

(defn minus ([x] (* x -1))
  ([x y] (- x y))
  )

(defn count-params
  ([] 0)
  ([x] 1)
  ([x y] 2)
  ([x y & more] (reduce (fn [yht a] (inc yht)) 2 more))
  )

(defn my-*
  ([] 1)
  ([x] x)
  ([x y & more] (reduce * (* x y) more))
  )

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more))
  )

(defn my-map [f a-seq]
  [:-])
