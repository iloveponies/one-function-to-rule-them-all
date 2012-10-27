(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
  	(reduce (fn [s1 s2] (str s1 " " s2)) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [v a] (conj v x a)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [n x] (inc n)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [rev x] (cons x rev)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[mn mx] x] [(min mn x) (max mx x)])
          [(first a-seq) (first a-seq)]
          a-seq))

(defn insert [sorted-seq n]
  (loop [inits []
         sq sorted-seq]
    (if (or (empty? sq) (>= (first sq) n))
      (concat inits (cons n nil) sq)
      (recur (conj inits (first sq)) (rest sq)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (letfn [(toggle [a-set elem]
            (if (contains? a-set elem)
              (disj a-set elem)
              (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] true))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & pn]
    (fn [x] (reduce (fn [b pi] (and b (pi x)))
                    (and (p1 x) (p2 x))
                    pn))))

(defn my-map ([f & seqs]
  (letfn [(xrest [seqs]
            (reduce (fn [ret sqi] (conj ret (rest sqi))) [] seqs))
          (xempty? [seqs]
            (reduce (fn [ret sqi] (or ret (empty? sqi))) false seqs))
          (xfirst [seqs]
            (reduce (fn [ret sqi] (conj ret (first sqi))) [] seqs))]
    (loop [ret []
           sqs seqs]
      (if (xempty? sqs)
        ret
        (recur (conj ret (apply f (xfirst sqs))) (xrest sqs)))))))