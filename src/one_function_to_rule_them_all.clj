(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
 (let [str-fy (fn [result y]
                 (if (empty? result)
                     y
                     (str result " " y)))]
    (reduce str-fy "" a-seq)))

(defn my-interpose [x a-seq]
  (let [helper (fn [result y]
                 (if (empty? result)
                     (concat [y] [])
                     (concat result (conj [x] y))))]
    (reduce helper () a-seq)))

(defn my-count [a-seq]
  (let [helper (fn [result y]
                   (inc result))]
    (reduce helper 0 a-seq)))

(defn my-reverse [a-seq]
  (let [helper (fn [result y]
                 (cons y result))]
    (reduce helper () a-seq)))

(defn min-max-element [a-seq]
  (let [helper (fn [result y]
                  (cond (empty? result) [y y]
                        (< y (get result 0)) (assoc result 0 y)
                        (> y (get result 1)) (assoc result 1 y)
                        :else result))]
    (reduce helper [] a-seq)))



(defn insert [sorted-seq n]
  (let [find-place (fn [result a-seq]
                     (cond (empty? a-seq) (concat result [n])
                           (> (first a-seq) n) (concat result [n] a-seq)
                           :else (recur (concat result [(first a-seq)]) (rest a-seq))))]

    (find-place [] sorted-seq)))

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

(defn count-params
  ([& more]
   (let [helper (fn [n x]
                           (inc n))]
     (reduce helper 0 more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & more] (reduce * x more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred1] (fn [x] (if (pred1 x) true false)))
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))


(defn my-map
  ([f a-seq] (loop [acc '()
                    b-seq a-seq]
               (if (empty? b-seq)
                    acc
                    (recur (concat acc [(f (first b-seq))]) (rest b-seq)))))
  ([f a-seq b-seq] (loop [acc '()
                          c-seq a-seq
                          d-seq b-seq]
                     (if (or (empty? c-seq) (empty? d-seq))
                         acc
                         (if (coll? (first c-seq))
                             (recur (concat acc (list (apply f (first d-seq) (first c-seq)))) (rest c-seq) (rest d-seq))
                             (recur (concat acc (list (f (first c-seq) (first d-seq)))) (rest c-seq) (rest d-seq))))))


  ([f a-seq b-seq & more] (reduce (fn [x y] (my-map f x y)) (my-map f a-seq b-seq) more)))
