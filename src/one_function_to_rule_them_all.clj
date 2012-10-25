(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [strc (fn [a b] (str a " " b))]
      (reduce strc a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (let [cat (fn [f r] 
				(if (empty? f)
                  (conj f r)
                  (conj (conj f x) r)))]
      (reduce cat [] a-seq))))

(defn my-count [a-seq]
  (let [c (fn [a b] (inc a))]
    (reduce c 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev (fn [a b] 
              (concat [b] a))]
    (reduce rev [] a-seq)))

(defn min-max-element [a-seq]
  (let [trackmm (fn [[mn mx] b]
                  (if (= mn mx nil)
                    [b b]
                    [(min mn b) (max mx b)]))]
    (reduce trackmm [nil nil] a-seq)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (cons n sorted-seq)
    (loop [fi '()
           re sorted-seq]
      (if (or (empty? re) (> (first re) n))
        (concat (reverse fi) (cons n re))
        (recur (cons (first re) fi) (rest re))))))

(defn insertion-sort [a-seq]
 (let [reuse insert
       recycle []]
   (reduce reuse recycle a-seq)))

(defn parity [a-seq]
  (let [join (fn [a b] 
               (if (contains? a b)
                 (disj a b)
                 (conj a b)))]
    (reduce join #{} a-seq)))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& x] 
   (my-count x)))

(defn my-*
 ([& nums]
  (reduce * 1 nums)))

(defn pred-and 
  ([] (fn [x] true))
  ([& preds]
   (let [andpred (fn [a b] (fn [x] (and (a x) (b x))))]
     (reduce andpred (pred-and) preds ))))

(defn my-map
  ([f & seqs]
   (loop [sequences seqs
          results '()]
     (if (every? empty? sequences)
       results
       (let [takef (fn [a b] 
                     (concat a (cons (first b) '())))
             taker (fn [a b]
                     (concat a (cons (rest b) '())))]
         (recur 
          (reduce taker '() sequences)
          (concat results 
                  (cons 
                   (apply f (reduce takef [] sequences)) '()))))))))