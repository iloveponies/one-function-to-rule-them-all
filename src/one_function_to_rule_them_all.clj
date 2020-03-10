(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (= 0 (count a-seq))
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [a b] (conj a x b)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc k] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc k] (concat [k] acc)) [] a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [values k] (let [min-value (min (first values) k)
                               max-value (max (second values) k)]
                           [min-value max-value]))
          [(first a-seq) (first a-seq)]
          a-seq))

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) [n]
    (< n (first sorted-seq)) (apply conj [n] sorted-seq)
    (> n (last sorted-seq)) (conj sorted-seq n)
    :else (loop [pos 0]
            (let [next-pos (inc pos)]
              (if (<= (get sorted-seq pos) n (get sorted-seq next-pos))
                (apply conj (conj (subvec sorted-seq 0 next-pos) n) (subvec sorted-seq next-pos))
                (recur next-pos))))))

(defn insertion-sort [a-seq]
  (reduce (fn [a-seq item] (insert a-seq item)) [] a-seq))

(defn parity [a-seq]
  (reduce (fn [a-set item] (if (odd? (count (filter (fn [x] (= item x)) a-seq)))
                             (conj a-set item)
                             a-set))
          #{}
          a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (reduce (fn [count _] (inc count)) 0 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & more] (reduce * x more)))

;stopped here
(defn pred-and
  ([] (fn [k] true))
  ([p] p)
  ([p & more-predicates] (fn [k]
                           (reduce (fn [result predicate]
                                     (if (not result)
                                       false
                                       (and (predicate k) result)))
                                   (p k)
                                   more-predicates))))

(defn convert-sequences [a-seq]
  (loop [i 0
         result []]
    (if (== i (count (first a-seq)))
      result
      (recur (inc i) (conj result (reduce (fn [acc item] (conj acc (get item i))) [] a-seq))))))

(defn my-map
  ([f a-seq]
   (seq (reduce (fn [result item] (conj result (f item))) [] a-seq)))
  ([f a-seq & more]
   (let [converted (convert-sequences (cons a-seq more))]
     (seq (reduce (fn [acc item] (conj acc (apply f item))) [] converted)))))
