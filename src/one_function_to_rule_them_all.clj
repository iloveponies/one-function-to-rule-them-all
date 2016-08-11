(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (let [add-space (fn [acc x]
                    (str acc (str " " x)))]
    (if (empty? a-seq)
      ""
      (reduce add-space a-seq))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-interpose [x a-seq]
  (let [add-elem (fn [acc nex]
                   (conj (conj acc nex) x))]
    (cond
      (empty? a-seq) '()
      ;(singleton? a-seq) (list (first a-seq))
      :else (rest (reduce add-elem '() (reverse a-seq))))))

(defn my-count [a-seq]
  (let [counter (fn [n x]
                  (inc n))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [fun (fn [a-seq elem]
              (let [mi (min (first a-seq) elem)
                    ma (max (second a-seq) elem)]
                [mi ma]))]
    (reduce fun [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (let [insert-helper (fn [index sorted-seq n]
                         (cond
                           (= index (count sorted-seq)) (concat sorted-seq (list n))
                           (< n (nth sorted-seq index)) (concat (take index sorted-seq)
                                                                (list n)
                                                                (drop index sorted-seq))
                           :else (recur (inc index)
                                        sorted-seq
                                        n)))]
    (if (empty? sorted-seq)
      (list n)
      (insert-helper 0 sorted-seq n))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

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
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x]
             (and (p1 x)
                  (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn give-next [seqs]
  (let [give-firsts (fn [acc x]
                      (conj acc (first x)))
        give-nexts (fn [acc x]
                     (conj acc (rest x)))]
    (if (empty? (first seqs))
      []
      (cons (reduce give-firsts [] seqs)
            (give-next (reduce give-nexts [] seqs))))))

(defn my-map
  ([f a-seq] (let [helper (fn [acc x]
                            (conj acc (f x)))]
               (reduce helper [] a-seq)))
  ([f a-seq & more] (let [take-first-elems (fn [acc x]
                                             (conj acc (first x)))
                          apply-to-first (fn [acc x]
                                           (conj acc (apply f x)))]

                      (reverse (reduce apply-to-first '() (give-next (cons a-seq more)))))))
