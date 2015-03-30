(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
  (reduce (fn [st1 st2] (str st1 " " st2 )) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [st1 st2] (conj (conj st1 x) st2)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [a b] (+ a 1)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq) (list n)
   (< n (first sorted-seq)) (conj (seq sorted-seq) n)
   :else (conj (insert (rest sorted-seq) n) (first sorted-seq))
   )
  )

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [s e] (if (contains? s e) (disj s e) (conj s e) ) ) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y))
  )

(defn count-params
  [& more] (count more)
  )

(defn my-*
  [& more]
  (reduce * 1 more))

(defn pred-and [& more]
  (reduce (fn [pr1 pr2] (fn [x] (and (pr1 x) (pr2 x))) ) (fn [x] true) more)
  )

(defn my-map [f & more]
  [:-])
