(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (let [add (fn [a b]
              (str a " " b))]
    (if (empty? a-seq)
      ""
      (reduce add a-seq))))

(defn my-interpose [x a-seq]
  (let [pose (fn [a b] (if (= nil b)
                         a
                         (conj a x b)))]
    (if (empty? a-seq)
      '()
      (seq (reduce pose (conj [] (first a-seq)) (rest a-seq))))))


(defn my-count [a-seq]
  (let [inc (fn [a b] (+ 1 a))]
  (reduce inc 0 a-seq)))

(defn my-reverse [a-seq]
  (let [adder (fn [a b] (concat (conj [] b) a))]
    (if (empty? a-seq)
      '()
      (reduce adder (conj [] (first a-seq)) (rest a-seq)))))

(defn min-max-element [a-seq]
  (let [minmax (fn [v e]
                 (cond
                   (< e (first v)) (assoc v 0 e)
                   (> e (second v)) (assoc v 1 e)
                   :else v))]
    (reduce minmax (conj [] (first a-seq) (first a-seq)) (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [seq sorted-seq
         seqq []]
    (if (empty? seq)
      (conj seqq n)
      (if (> (first seq) n)
        (concat (conj seqq n) seq)
        (recur (rest seq) (conj seqq (first seq)))))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [v elem]
                 (if (contains? v elem)
                   (disj v elem)
                   (conj v elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (let [add (fn [v b] (+ v 1))]
    (reduce add 0 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [v] true))
  ([x] (fn [v] (x v)))
  ([x y] (fn [v] (and (x v) (y v))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn my-map
  ([f a-seq] (loop [seq a-seq
                    seqq []]
               (if (empty? seq)
                 seqq
                 (recur (rest seq) (conj seqq (f (first seq)))))))
  ([f a-seq b-seq] (loop [c-seq a-seq
                          d-seq b-seq
                          seqq []]
                     (if (empty? c-seq)
                       seqq
                       (recur (rest c-seq) (rest d-seq) (conj seqq (f (first c-seq) (first d-seq)))))))
  ([f a-seq b-seq & more] (loop [mor more
                                 a a-seq
                                 b b-seq]
                            (if (empty? mor)
                              (my-map f a b)
                              (recur (rest mor) (my-map f a b) (first mor))))))
