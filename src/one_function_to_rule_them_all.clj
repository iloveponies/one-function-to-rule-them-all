(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn strSpace [a b]
  (cond
   (empty? a) b
   (empty? b) a
   :else (str a " " b)))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce strSpace a-seq)))

(defn my-interpose [x a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (reduce (fn [a b] (if (empty? a) (conj a b) (conj (conj a x) b))) [] a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [count e] (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [revSeq (fn [lst elem] (conj lst elem))]
    (reduce revSeq () a-seq)))

(defn min-max-element [a-seq]
  (let [minMax (fn [[minE maxE] elem]
                 (let [newMin (if (< elem minE) elem minE)
                       newMax (if (> elem maxE) elem maxE)]
                   [newMin newMax]))]
    (reduce minMax '[100000, -100000] a-seq)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (conj '() n)
    (if (< n (first sorted-seq))
      (cons n sorted-seq)
      (conj (insert (rest sorted-seq) n) (first sorted-seq)))))

(defn insertion-sort [a-seq]
  (reduce insert '[] a-seq))

(defn parity [a-seq]
  (let [freqs (frequencies a-seq)]
    (set (keys (filter (fn [x] (= (mod (second x) 2) 1)) freqs)))))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [k] true))
  ([f1] (fn [k] (f1 k)))
  ([f1 f2] (fn [k] (and (f1 k) (f2 k))))
  ([f1 f2 & more] (reduce pred-and (pred-and f1 f2) more)))

(filter (pred-and number? integer? pos?)
        [1 0 -2 :a 7 "a" 2])


(defn reducen [f initial seqs]
  (if (empty? (first seqs))
    initial
    (recur f
           (conj initial (apply f (map first seqs)))
           (map rest seqs))))

(defn my-map [f & seqs]
  (reducen f [] seqs))




