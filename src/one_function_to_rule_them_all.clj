(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce (fn [a b] (if (= (* 2 (- (count a-seq) 1)) (count a))
                      (reverse (conj (reverse a) b))
                      (reverse (conj (conj (reverse a) b) x)))) '() a-seq))

(defn my-count [a-seq]
  (reduce (fn [c elem] (inc c)) 0 a-seq))

(defn my-reverse [a-seq]
    (reduce (fn [rev elem] (conj rev elem)) '() a-seq))

(defn min-max-element [a-seq]
    [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (loop [result '()
         inserted false
         b-seq (reverse sorted-seq)]
    (cond
      (and (not inserted) (empty? b-seq)) (recur (conj result n) true b-seq)
      (empty? b-seq) result
      (or inserted (< n (first b-seq))) (recur (conj result (first b-seq)) inserted (rest b-seq))
      :else (recur (conj result n) true b-seq))))


(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] (if (some #(= elem %) a-set)
                                  (disj a-set elem)
                                  (conj a-set elem)))]
    (reduce toggle '#{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([& more]
   (reduce (fn [c elem] (inc c)) 0 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p r] (fn [x] (and (p x) (r x))))
  ([p r & more]
   (reduce pred-and (pred-and p r) more)))

(defn add-to-end [x a-seq]
  (reverse (conj (reverse a-seq) x)))

(defn has-empty-subseq [a-seq]
  (loop [b-seq a-seq]
    (cond
      (empty? b-seq) false
      (empty? (first b-seq)) true
      :else (recur (rest b-seq)))))


(defn without-firsts [a-seq]
  (if (empty? a-seq)
    '()
    (loop [res '()
            b-seq a-seq]
      (if (empty? b-seq)
      res
      (recur (add-to-end (rest (first b-seq)) res) (rest b-seq))))))

(defn corresponding [a-seq]
  (loop [result '()
         mo a-seq]
    (if (has-empty-subseq mo)
      result
      (recur
        (add-to-end (reduce (fn [fs b-seq] (add-to-end (first b-seq) fs)) '() mo) result)
        (without-firsts mo)))))

(defn my-map
  ([f a-seq]
   (reduce (fn [prev elem] (reverse (conj (reverse prev) (f elem)))) '() a-seq))
  ([f a-seq & more]
   (reduce (fn [prev b-seq] (add-to-end (apply f b-seq) prev)) '() (corresponding (conj more a-seq)))))
