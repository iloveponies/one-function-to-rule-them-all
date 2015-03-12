(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc next] (str acc " " next)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (cons (first a-seq) (reduce (fn [acc next] (conj (conj acc x) next)) [] (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [acc _] (+ acc 1)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc next] (cons next acc)) [] a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [acc next] [(min next (first acc)) (max next (second acc))]) [(first a-seq) (first a-seq)] (rest a-seq)))

(defn insert [sorted-seq n]
  (loop [prev []
         next sorted-seq]
    (cond
      (empty? next) (conj prev n)
      (<= n (first next)) (concat (conj prev n) next)
      :else (recur (conj prev (first next)) (rest next)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([& xs] (count xs)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [_] true))
  ([p] p)
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more] (reduce pred-and (pred-and p q) more)))

(defn my-map [f & seqs]
  (loop [mapped []
         curr-seqs seqs]
    (if (not (every? (complement empty?) curr-seqs))
      mapped
      (let [split (reduce (fn [acc next] [(conj (first acc) (first next)) (conj (second acc) (rest next))]) [[] []] curr-seqs)]
        (recur (conj mapped (apply f (first split))) (second split))))))
