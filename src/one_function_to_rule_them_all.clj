(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc elem] (str acc " "  elem)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [acc elem] (conj acc x elem)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [elem-count _]
                  (inc elem-count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [acc elem]
                   (cons elem acc))]
    (reduce reverser '() a-seq)))

(defn min-max-element [a-seq]
  (let [min-max (fn [acc elem]
                  (cond
                    (> elem (last acc)) [(first acc) elem]
                    (< elem (first acc)) [elem (last acc)]
                    :else acc))]
    (reduce min-max [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (if (or (empty? sorted-seq) (< n (first sorted-seq)))
    (cons n sorted-seq)
    (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce (fn [acc elem]
            (toggle acc elem))
          #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& more]
  (reduce * more))

(defn pred-and [& more]
  (fn [x] (reduce (fn [acc elem] (and acc (elem x))) true more)))

(defn my-map
  ([f a-seq]
   (seq (reduce (fn [acc elem] (conj acc (f elem))) [] a-seq)))

  ([f a-seq & more]
   (loop [acc []
          rest-seqs (cons a-seq more)]
     (if (empty? (first rest-seqs))
       (seq acc)
       (recur (conj acc (apply f (my-map first rest-seqs))) (my-map rest rest-seqs))))))
