(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [cate (fn [m n]
                (str m " " n))]
      (reduce cate a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (let [ad   (fn [m n]
                (conj m x n))]
      (reduce ad [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [ counts (fn [n elem]
                    (if (empty? elem)
                      n
                      (inc n)))]
    (reduce counts 0 a-seq)))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    '()
    (reduce conj '() a-seq)))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (let [min-max-helper (fn [[min max] elem]
                          (let [[min max]
                            (if (> elem max)
                              [min elem]
                              [min max])))]
                             (if (< elem min)
                                  [elem max]
                                  [min max])]
      (reduce min-max-helper [(first a-seq) (first a-seq)] a-seq))))

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq)       (seq [n])
    (< n (first sorted-seq))  (apply conj [] n sorted-seq)
    :else (loop [a-seq sorted-seq
           new-seq []]
      (if (empty? a-seq)
        new-seq
        (cond
          (and (<= (first a-seq) n) (empty? (rest a-seq)))
            (conj new-seq (first a-seq) n)
          (and (<= (first a-seq) n) (>= (first (rest a-seq)) n))
            (apply conj new-seq (first a-seq) n (rest a-seq))
          :else (recur (rest a-seq) (conj new-seq (first a-seq))))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))


(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& pars]
  (count pars))

(defn my-* [& pars]
  (reduce * 1 pars))

(defn pred-and
  ([] (fn [x] true))
  ([x1] (fn [x] (x1 x)))
  ([x1 x2] (fn [x] (and (x1 x) (x2 x))))
  ([x1 x2 & more]
    (reduce pred-and (pred-and x1 x2) more)))

(defn my-map
  ([f a-seq]
   (let [my-helper (fn [a-seq elem]
                   (conj a-seq (f elem)))]
     (seq (reduce my-helper [] a-seq))))
  ([f a-seq & more]
   (let [se (cons a-seq more)]
     (if (some empty? se)
       '()
       (cons (apply f (my-map first se))
                  (apply my-map f (my-map rest se)))))))
