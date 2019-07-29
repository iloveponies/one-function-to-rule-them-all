(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
      ""
      (reduce (fn [c1 c2] (str c1 " " c2)) a-seq)))

(defn my-interpose [x a-seq]
 (if (empty? a-seq)
      '()
      (reduce (fn [c1 c2] (conj (conj c1 x ) c2)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [x _] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [c1 c2] (cons c2 c1)) [] a-seq) )


(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (let [parts (split-with (partial >= n) sorted-seq)]
       (concat (first parts) [n] (second parts))))




  ; Didn't read instructions through and tried with reduce, still think i was pretty close? Gives runtime error
  ;(reduce (fn [c1 c2] ((if  (>= c2 n)
  ;                         (concat (concat c1 [n]) [c2])
  ;                         (concat c1 [c2])))) sorted-seq))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
     (disj a-set elem)
     (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)) )

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
     (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [_] true))
  ([p1] (fn [x] (p1 x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
     (fn [x] (reduce (fn [f1 f2] (and f1 (f2 x))) ((pred-and p1 p2) x) more))))

(defn my-map [f a-seq]
  )
