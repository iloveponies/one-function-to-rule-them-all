(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reverse (reduce (fn [head new-elem] (conj head x new-elem)) (cons (first a-seq) '()) (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [c _] (inc c)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [v elem] (cons elem v)) '() a-seq))

(defn min-max-element [a-seq]
  [(reduce (fn [mmin new-elem] (min mmin new-elem)) a-seq)
   (reduce (fn [mmax new-elem] (max mmax new-elem)) a-seq)])

(defn insert [sorted-seq n]
  (loop [left '()
         right sorted-seq]
    (if (empty? right)
      (concat left [n])
      (let [fst (first right)
            rst (rest right)]
        (if (<= n fst)
          (concat left [n] right)
          (recur (concat left [fst]) rst))))))

(defn insertion-sort [a-seq]
  (reduce (fn [sorted elem] (insert sorted elem)) '() a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  [& params] (count params))

(defn my-*
  ([] 1)
  ([x] (* x))
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map
  ([f a-seq] (seq (reduce (fn [lst elem] (conj lst (f elem))) [] a-seq)))
  ([f a-seq & more] (let [n (count a-seq)
                          merged-seqs (apply interleave (cons a-seq more))
                          partitions (partition n merged-seqs)
                          helper-fn (fn [lst elem] (conj lst (apply f elem)))]
                      (seq (reduce helper-fn [] partitions)))))

