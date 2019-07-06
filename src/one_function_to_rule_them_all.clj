(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc s]
              (.concat acc (.concat " " s)))
            a-seq)))

(defn my-interpose [x a-seq]
  (cond
   (empty? a-seq) a-seq
   :else (reduce (fn [acc v]
                   (conj acc x v))
                 [(first a-seq)]
                 (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[lil big] x]
            [(min x lil) (max x big)])
          (vec (repeat 2 (first a-seq)))
          (rest a-seq)))

(defn insert [sorted-seq n]
  (loop [acc []
         xs sorted-seq]
    (if (empty? xs)
      (conj acc n)
      (let [x (first xs)]
        (if (> x n)
          (concat (conj acc n) xs)
          (recur (conj acc x) (rest xs)))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & etc] (reduce * (my-* x y) etc)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p-1 p-2] (fn [x] (and (p-1 x) (p-2 x))))
  ([p-1 p-2 & etc] (reduce (fn [acc p]
                             (fn [x]
                               (and (acc x) (p x))))
                           (pred-and p-1 p-2)
                           etc)))

(defn my-map
  ([f xs]
     (loop [acc []
            xs' xs]
       (if (empty? xs')
         acc
         (recur (conj acc (f (first xs')))
                (rest xs')))))
  ([f xs ys]
     (loop [acc []
            xs' xs
            ys' ys]
       (if (or (empty? xs') (empty? ys'))
         acc
         (let [v (f (first xs') (first ys'))]
           (recur (conj acc v)
                  (rest xs')
                  (rest ys'))))))
  ([f xs ys & etc]
     (reduce (fn [acc ws]
               (loop [results []
                      acc' acc
                      ws' ws]
                 (if (or (empty? acc') (empty? ws'))
                   results
                   (let [args (if (coll? (first acc')) ; hack?
                                (conj (first acc') (first ws'))
                                [(first acc') (first ws')])
                         v (apply f args)]
                     (recur (conj results v)
                            (rest acc')
                            (rest ws'))))))
             (my-map f xs ys)
             etc)))
