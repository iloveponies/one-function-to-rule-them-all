(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [s1 s2] (str s1 " " s2)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (rest (reduce (fn [sq t] (conj sq x t)) [] a-seq))))

(defn my-count [a-seq]
  (let [my-count-helper (fn [cnt _] (inc cnt))]
    (reduce my-count-helper 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce (fn [sq x] (concat [x] sq)) [] a-seq))

(defn min-max-element [a-seq]
  (let [min-max-helper (fn [[xmin xmax] x] [(min x xmin) (max x xmax)])]
    (reduce min-max-helper (repeat 2 (first a-seq)) (rest a-seq))))

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq)
   [n]
   (< n (first sorted-seq))
   (cons n sorted-seq)
   :else
   (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [a-set x] (if (contains? a-set x) (disj a-set x) (conj a-set x))) #{} a-seq))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (reduce (fn [cnt _] (inc cnt)) 0 more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [_] true))
  ([pred] pred)
  ([pred1, pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1, pred2 & more] (fn [x] (reduce (fn [tf, pred] (and tf (pred x))) ((pred-and pred1 pred2) x) more))))

(defn my-first-helper [seq-of-seqs]
  (if (empty? seq-of-seqs)
    ()
    (cons (first (first seq-of-seqs)) (my-first-helper (rest seq-of-seqs)))))

(defn my-rest-helper [seq-of-seqs]
  (if (empty? seq-of-seqs)
    ()
    (cons (rest (first seq-of-seqs)) (my-rest-helper (rest seq-of-seqs)))))

(defn my-map [f & more]
  (if (empty? (first more))
      (first more)
      (cons (apply f (my-first-helper more)) (apply my-map (cons f (my-rest-helper more))))))
