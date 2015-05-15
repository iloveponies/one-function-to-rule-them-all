(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [st a] (str st " " a)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reverse (reduce (fn [col a]
                       (conj (conj col x) a))
                     (seq [(first a-seq)]) (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [sum k] (inc sum)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [minmax el]
            [(min (first minmax) el) (max (last minmax) el)])
          [(first a-seq) (first a-seq)]
          a-seq))

(defn insert [sorted-seq n]
  (loop [a '()
         b sorted-seq]
    (if (empty? b)
      (concat a [n])
      (if (< (first b) n)
        (recur (concat a [(first b)])
               (rest b))
        (concat a (concat [n] b))))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce (fn [a el]
            (if (contains? a el)
              (disj a el)
              (conj a el)))
          #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more] (reduce pred-and (pred-and p q) more)))

(defn my-map [f & x]
  (if (some empty? x)
    '()
    (cons
     (apply f (reduce (fn [a b] (concat a [(first b)])) '() x))
     (apply my-map (cons f (reduce (fn [a b] (conj a (rest b))) '() x))))))



