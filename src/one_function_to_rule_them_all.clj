(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [string value] (str string " " value)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [coll value] (conj coll x value)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [cnt _] (inc cnt)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [coll v] (cons v coll)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [minmax v]
            [(min v (first minmax))
             (max v (second minmax))])
          [(first a-seq) (first a-seq)]
          (rest a-seq)))

(defn ins-help [a-seq]
  (if (empty? a-seq) a-seq (cons (first a-seq) (ins-help (rest a-seq)))))

(defn insert [sorted-seq n]
    (if (empty? sorted-seq)
      (cons n sorted-seq)
      (let [a (first sorted-seq)]
        (if (< n a)
          (cons n (ins-help sorted-seq))
          (cons a (insert (rest sorted-seq) n))))))

(defn insertion-sort [a-seq]
  (reduce (fn [coll v] (insert coll v)) [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (let [op (if (contains? a-set elem)
                            disj
                            conj)]
                   (op a-set elem)))]
  (reduce (fn [coll v] (toggle coll v)) #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x & xs] (reduce (fn [cnt _] (inc cnt)) 1 xs)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & dots] (* x y (reduce (fn [prod v] (* prod v)) 1 dots))))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x]
             (and (p1 x)
                  (p2 x))))
  ([p1 p2 & ps] (fn [x]
                  (reduce
                    (fn [bool v]
                      (and bool (v x)))
                    (and (p1 x)
                         (p2 x))
                    ps))))

(defn combo-seq-filter [a-seq]
  (cond
    (empty? a-seq) '()
    (empty? (first a-seq)) '()
    :else (cons (reduce (fn [coll seqq]
                          (conj coll (first seqq)))
                        []
                        a-seq)
                (combo-seq-filter (reduce (fn [coll seqq]
                                            (conj coll (rest seqq)))
                                          []
                                          a-seq)))))
(defn my-map
  ([f a-seq] (if (empty? a-seq)
               '()
               (cons (f (first a-seq))
                     (my-map f (rest a-seq)))))
  ([f a-seq & seqs] (let [combo-seq (combo-seq-filter (conj seqs a-seq))]
                      (my-map (fn [x]
                                (apply f x)) combo-seq))))

