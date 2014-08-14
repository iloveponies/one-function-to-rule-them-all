(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (< (count a-seq) 2)
    (str (first a-seq))
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [a b] (conj a x b)) [] a-seq)))

(defn my-count [a-seq]
  (let [helper (fn [n elem]
                 (if elem (inc n) n))]
    (reduce helper 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce (fn [rev elem] (cons elem rev)) () a-seq))

(defn min-max-element [a-seq]
  (let [helper (fn [[sm gr] elem]
                 [(min sm elem) (max gr elem)])
        init (first a-seq)]
    (reduce helper [init init] a-seq)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
    (loop [i 0]
      (cond
       (= (count sorted-seq) i)
         (conj sorted-seq n)
       (< n (sorted-seq i))
         (vec (concat (take i sorted-seq) [n] (drop i sorted-seq)))
       :else
         (recur (inc i))))))

(defn insertion-sort [a-seq]
  (loop [sorted []
         a-seq a-seq]
    (if (empty? a-seq)
      sorted
      (recur (insert sorted (first a-seq)) (rest a-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [pset #{}
         a-seq a-seq]
    (if (empty? a-seq)
      pset
      (recur (toggle pset (first a-seq)) (rest a-seq)))))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more] (reduce pred-and (pred-and p q) more)))

(defn transpose [vectors]
  (let [helper (fn [acc vecs]
                 (if (empty? (flatten vecs))
                   acc
                   (recur (conj acc (map first vecs)) (map rest vecs))))]
    (helper [] vectors)))

(defn my-map
  ([f a-seq]
   (reduce (fn [acc x] (conj acc (f x))) [] a-seq))
  ([f a-seq & more]
   (my-map (fn [v] (apply f v)) (transpose (cons a-seq more)))))
