(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [s t] (str s " " t)) a-seq)))

(defn my-interpose [x a-seq]
  (if (or (== (count a-seq) 1) (empty? a-seq))
    a-seq
    (reduce (fn [a b] (if (not (coll? a)) (conj [] a x b) (conj a x b))) a-seq)))

(defn my-count [a-seq]
  (reduce (fn [count e] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [a b] (if (not (coll? a))
                        (cons b (cons a '()))
                        (cons b a))) a-seq)))

(defn min-max-element [a-seq]
  (reduce (fn [avec e] [(min (get avec 0) e) (max (get avec 1) e)])
            [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (let [leng (count sorted-seq)
        avec (vec sorted-seq)]
    (loop [x 0]
      (cond
       (>= x leng) (conj avec n)
       (<= n (get avec x)) (concat (take x avec)
                                         [n] (drop x avec))
       :else (recur (inc x))))))

(defn insertion-sort [a-seq]
  (if (or (== (count a-seq) 1) (empty? a-seq))
    a-seq
    (reduce insert [(first a-seq)] (drop 1 a-seq))))

(defn parity [a-seq]
  (reduce (fn [aset x] (if (contains? aset x)
                         (disj (set aset) x)
                         (conj (set aset) x))) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x & more]
     (reduce (fn [count y] (inc count)) 1 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y]
     (loop [z 0, c 0] (if (>= c y) z (recur (+ z x) (inc c)))))
  ([x y & more]
     (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more]
     (reduce pred-and (pred-and p q) more)))

(defn my-map
  ([f a-seq]
     (reduce (fn [seqq x] (concat seqq [(do (f x))]))
             [(do (f (first a-seq)))] (rest a-seq)))
  ([f a-seq b-seq]
     (loop [seqp a-seq
            seqq b-seq
            res '()]
       (if (or (empty? seqp) (empty? seqq))
         res
         (recur (rest seqp)
                (rest seqq)
                (concat res [(cond
                              (and (coll? (first seqp))
                                   (not (coll? (first seqq))))
                              (apply f (conj (first seqp) (first seqq))),
                                   (and (coll? (first seqp)) (coll? (first
                                                                     seqq)))
                                   (apply f (concat (first seqp) (first seqq))),
                                   (and (not (coll? (first seqp)))
                                        (coll? (first seqq)))
                                   (apply f (cons (first seqp) seqq))
                               :else (f (first seqp) (first seqq)))])))))
  ([f a-seq b-seq & more]
     (reduce (fn [seqp seqq] (my-map f seqp seqq)) (my-map f a-seq b-seq) more)))
