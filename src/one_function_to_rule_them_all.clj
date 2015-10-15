(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc word] (str acc " " word)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce
     (fn [a-vector elem] (conj a-vector x elem))
     [(first a-seq)]
     (rest a-seq))))


(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a-seq' elem] (conj a-seq' elem)) () a-seq))

(defn min-max-element [a-seq]
  (let [is-min (fn [value [min' _]] (< value min'))
        is-max (fn [value [_ max']] (> value max'))
        initial-min-max [(first a-seq) (first a-seq)]
        remaing-seq (rest a-seq)]
    (reduce
     (fn
       [seq-min-max value]
       (cond
        (is-min value seq-min-max) [value (second seq-min-max)]
        (is-max value seq-min-max) [(first seq-min-max) value]
        :else seq-min-max))
     initial-min-max
     remaing-seq)))

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq) (cons n ())
   (< n (first sorted-seq)) (cons n sorted-seq)
   :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce (fn [a-seq' elem] (insert a-seq' elem)) () a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce (fn [a-set elem] (toggle a-set elem)) #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more]
   (fn [x]
     (reduce (fn [acc pred] (and acc (pred x))) (and (p x) (q x)) more))))

(defn my-map [f & seqs]
  (if (some empty? seqs)
    '()
    (cons
     (apply f (reduce (fn [x y] (concat x [(first y)])) '() seqs))
     (apply my-map (cons f (reduce (fn [x y] (conj x (rest y))) '() seqs))))))
