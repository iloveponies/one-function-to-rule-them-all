(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (let [helper (fn [x y] (str x " " y))]
    (if (empty? a-seq)
      ""
      (reduce helper a-seq))))

(defn my-interpose [x a-seq]
  (let [helper (fn [y z] (conj (conj y z) x))]
    (drop-last (reduce helper [] a-seq))))

(defn my-count [a-seq]
  (let [helper (fn [c e] (inc c))]
    (reduce helper 0 a-seq)))

(defn my-reverse [a-seq]
  (let [helper (fn [x y] (cons y x))]
    (reduce helper [] a-seq)))

(defn min-max-element [a-seq]
  (let [min-helper (fn [x y] (min x y))
        max-helper (fn [x y] (max x y))]
    [(reduce min-helper (first a-seq) a-seq)
     (reduce max-helper (first a-seq) a-seq)]))

(defn insert [sorted-seq n]
  (loop [a-seq sorted-seq
         b-seq []]
    (cond
     (empty? a-seq) (conj b-seq n)
     (>= (first a-seq) n) (concat (conj b-seq n) a-seq)
     :else (recur (rest a-seq) (conj b-seq (first a-seq))))))

(defn insertion-sort [a-seq]
  (let [helper (fn [x y] (insert x y))]
    (reduce helper [] a-seq)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (let [helper (fn [x y] (toggle x y))]
    (reduce helper #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (let [helper (fn [y z] (inc y))]
    (reduce helper 0 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more]
   (reduce pred-and (pred-and p q) more)))

(defn my-map-helper1 [f a-seq]
  (loop [b-seq a-seq
         c-seq []]
    (if (empty? b-seq)
      c-seq
      (recur (rest b-seq) (conj c-seq (f (first b-seq)))))))

(defn my-map-helper2 [f a-seq b-seq]
  (loop [c-seq a-seq
         d-seq b-seq
         e-seq []]
    (if (or (empty? c-seq) (empty? d-seq))
      e-seq
      (recur (rest c-seq)
             (rest d-seq)
             (concat e-seq (f (first c-seq) (first d-seq)))))))

(defn my-map
  ([f a-seq] (my-map-helper1 f a-seq))
  ([f a-seq & more]
   (reduce (fn [x y] (my-map-helper2 f x y)) a-seq more)))
   ;(reduce (fn [x y] (my-map f x y)) (my-map f a-seq b-seq) more)))

(defn get-firsts [& more]
   (loop [mores more
           result []]
      (if (empty? mores)
        result
        (recur (rest mores) (conj result (ffirst mores))))))

(defn get-rests [& more]
  (loop [mores more
         result []]
    (if (empty? mores)
      result
      (recur (rest mores) (conj result (rest (first mores)))))))

(defn my-map [f & more]
  (loop [firsts (apply get-firsts more)
         rests (apply get-rests more)
         result []]
    (if (or (empty? rests) (some empty? rests))
      (conj result (apply f firsts))
      (recur (apply get-firsts rests)
             (apply get-rests rests)
             (conj result (apply f firsts))))))
