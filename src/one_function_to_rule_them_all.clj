(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)
    ))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [acc el] (conj (conj acc x) el)) [] a-seq)))

(defn my-count [a-seq]
  (reduce + 0 (map (fn [x] 1)  a-seq)))


(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[i a] b]
            [(min i b)
             (max a b)])
          [(first a-seq) (first a-seq)]
          a-seq))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (seq [n])
    (if (> n (first sorted-seq))
      (cons (first sorted-seq)
            (insert (rest sorted-seq) n))
      (cons n
            sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce
   (fn [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))
   #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-* [& more]
  (reduce * 1 more))

(defn pred-and [& more]
  (fn [x]
    ((fn [x preds]
    (if (empty? preds)
      true
      (if ((first preds) x)
        (recur x (rest preds))
        false)))
     x more)))

(defn my-map1 [f a-seq]
  (reduce (fn [a b] (conj a (f b))) [] a-seq)
  )


(defn reducen [f initial seqs]
  (if (empty? (first seqs))
    initial
    (recur f
           (conj initial (apply f (my-map1 first seqs)))
           (my-map1 rest seqs))))

(defn my-map [f & seqs]
  (reducen f [] seqs))
