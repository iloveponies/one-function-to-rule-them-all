(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [prev curr] (str prev " " curr)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    `()
    (reduce (fn [prev curr] (conj prev x curr)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [counter item] (inc counter)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [revers item] (cons item revers)) `() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    nil
    (reduce (fn [res item] (list (min (first res) item) (max (second res) item))) (list (first a-seq) (first a-seq)) (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [res-seq []
         n n
         a-seq sorted-seq]
    (let [a-first (first a-seq)]
      (cond
        (empty? a-seq) (conj res-seq n)
        (<= n a-first) (concat res-seq [n] a-seq)
        :else (recur (conj res-seq a-first) n (rest a-seq))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set item]
  (if (contains? a-set item)
    (disj a-set item)
    (conj a-set item)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [i] true))
  ([pred] pred)
  ([pred-1 pred-2] (fn [item] (and (pred-1 item) (pred-2 item))))
  ([pred-1 pred-2 & preds]
   (fn [item]
     (if ((pred-and pred-1 pred-2) item)
       (loop [prev-res true
              preds preds]
         (cond
           (false? prev-res) false
           (empty? preds) true
           :else (recur (and prev-res ((first preds) item)) (rest preds))))))))

(defn my-map-single [f a-seq]
  (reduce (fn [res-seq item] (conj res-seq (f item))) [] a-seq))

(defn seqs-rests [seqs]
  (reduce (fn [res-seq a-seq]
            (let [a-rest (rest a-seq)]
              (if (empty? a-rest) res-seq (conj res-seq a-rest)))) [] seqs))

(defn my-map-more [res-seq f a-seqs]
  (if (empty? a-seqs)
    res-seq
    (let [firsts (my-map-single first a-seqs)
          rests (seqs-rests a-seqs)
          firsts-applyed (apply f firsts)]
      (recur (conj res-seq firsts-applyed) f rests))))

(defn my-map
  ([f a-seq] (my-map-single f a-seq))
  ([f a-seq & more] (my-map-more [] f (cons a-seq more))))
