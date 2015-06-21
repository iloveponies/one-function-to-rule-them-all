(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (let [intr-psd (reduce #(conj %1 x %2) '() a-seq)]
    (rest (reverse intr-psd))))

(defn my-count [a-seq]
  (let [counter (fn [x counts]
                  (inc x))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (let [maxi (reduce max a-seq)
        mini (reduce min a-seq)]
    [mini maxi]))

(defn insert [sorted-seq n]
  (cond (empty? sorted-seq) [n]
        (< n (first sorted-seq)) (cons n sorted-seq)
        :else
        (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (let [apu (fn [h t] (insert h t))]
    (reduce apu '() a-seq)))

(defn frequencies [a-seq]
  (let [apu
        (fn [acc x]
          (assoc acc x
            (inc
             (get acc x 0))))]
    (reduce apu {} a-seq)))

(defn parity [a-seq]
  (let [laskuri (frequencies a-seq)]
    (reduce
     (fn [al [bl cl]]
       (cond
        (odd? cl) (conj al bl )
        :else al))
     #{} laskuri)))

(defn minus
  ([x] (unchecked-negate-int x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x & more] (inc (count more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (* x y) more)))

(defn pred-and
  ([] (constantly true))
  ([x] x)
  ([x y] (fn [c] (and (x c) (y c))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn my-map
  ([f a] (map f a))
  ([f a b] (map f a b))
  ([f a b & more]
   (let [apu (fn apu [alphas]
     (let [ms (my-map seq alphas)]
       (when (every? identity ms)
       (cons (my-map first ms)
         (apu (my-map rest ms))))))]
           (my-map #(apply f %) (apu (conj more b a))))))
