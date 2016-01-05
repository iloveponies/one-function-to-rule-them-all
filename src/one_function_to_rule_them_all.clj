(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (rest (reduce (fn [a b] (conj a x b)) [] a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverse-seq (fn [reversed e]
                  (cons e reversed))]
    (reduce reverse-seq [] a-seq)))

(defn min-max-element [a-seq]
  (let [min-elem (reduce min a-seq)
        max-elem (reduce max a-seq)]
    [min-elem max-elem]))

(defn insert [sorted-seq n]
  (let [begin-seq (filter (fn [x] (< x n)) sorted-seq)
        end-seq (filter (fn [x] (> x n)) sorted-seq)]
    (concat begin-seq [n] end-seq)
  ))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set e]
                 (if (contains? a-set e)
                   (disj a-set e)
                   (conj a-set e)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [n] true))
  ([& more]
   (fn [n]
     (let [pred-helper (fn [bool pred] (and bool (pred n)))]
       (reduce pred-helper true more)))))

(defn my-map [f & more]
  (let [first-seq (fn [seqs] (reduce (fn [result-seq e]
                            (conj result-seq (first e))) '() seqs))
        rest-seqs (fn [seqs] (reduce (fn [result-seq e]
                            (conj result-seq (rest e))) '() seqs))]
    (loop [result-seqs '()
           remaining-seqs more]
      (if (some empty? remaining-seqs)
        (reverse result-seqs)
        (recur (cons (apply f (first-seq remaining-seqs)) result-seqs) (rest-seqs remaining-seqs))))))
