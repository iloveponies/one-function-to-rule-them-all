(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (let [add-space (fn [acc x]
                    (if (= x nil)
                      acc
                      (str acc " " x)))]
      (reduce add-space (str (first a-seq)) (rest a-seq))))

(defn my-interpose [x a-seq]
  (let [add-x (fn [acc y]
                (if (= y nil)
                  acc
                  (conj (conj acc x) y)))]
    (if (empty? a-seq)
      []
      (reduce add-x [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [count x] (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [cons-helper (fn [acc x] (cons x acc))]
    (reduce cons-helper [] a-seq)))

(defn min-max-element [a-seq]
  (let [min-max-helper (fn [[m1 m2] x]
                         (cond
                           (and (nil? m1) (nil? m2)) [x x]
                           (<= x m1) [x m2]
                           (>= x m2) [m1 x]
                           :else [m1 m2]))]
    (reduce min-max-helper [nil nil] a-seq)))

(defn insert [sorted-seq n]
  (let [insert-helper (fn [acc seq1 k]
                        (cond
                          (empty? seq1) (conj acc k)
                          (< k (first seq1)) (concat (conj acc k) seq1)
                          :else (recur (conj acc (first seq1)) (rest seq1) k)))]
    (insert-helper [] sorted-seq n)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [s e]
                 (if (contains? s e)
                   (disj s e)
                   (conj s e)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& params]
  (my-count params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [arg] (= arg arg)))
  ([x] (fn [arg] (x arg)))
  ([x y] (fn [arg] (and (x arg) (y arg))))
  ([x y & more] (fn [arg] ((reduce pred-and (pred-and x y) more) arg))))

(defn vec-firsts [acc args]
  (let [firsts (fn [acc vecs] (if (empty? vecs)
                                acc
                                (recur (conj acc (first (first vecs))) (rest vecs))))
        rests (fn [acc vecs] (if (empty? vecs)
                               acc
                               (recur (conj acc (rest (first vecs))) (rest vecs))))]
    (if (empty? (first args))
      acc
      (vec-firsts (conj acc (firsts [] args)) (rests [] args)))))

(defn my-map
  ([f lst] (reduce (fn [acc x] (conj acc (f x))) [] lst))
  ([f lst & more] (reduce (fn [acc x] (conj acc (apply f x))) [] (vec-firsts [] (cons lst more)))))
