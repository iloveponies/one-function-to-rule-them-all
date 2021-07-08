(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc elem] (str acc " " elem)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [acc elem] (conj acc x elem)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [s _](inc s)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %) '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq) []
  (let [f (first a-seq)]
    (reduce #(identity [(min (% 0) %2) (max (% 1) %2)]) [f f] (rest a-seq)))))

(defn insert-helper [sorted-seq n new-vec bool_inserted] (if (empty? sorted-seq)
                                                            (if bool_inserted
                                                              new-vec
                                                              (conj new-vec n))
                                                            (if (and (> (first sorted-seq) n) (not bool_inserted))
                                                              (insert-helper (rest sorted-seq) n (conj (conj new-vec n) (first sorted-seq)) true)
                                                              (insert-helper (rest sorted-seq) n (conj new-vec (first sorted-seq)) bool_inserted))))

(defn insert [sorted-seq n] (if (empty? sorted-seq)
                              (cons n '())
                              (insert-helper sorted-seq n [] false)))

(defn insertion-sort [a-seq] (reduce (fn [sorted-vec elem] (insert sorted-vec elem)) [] a-seq))

(defn parity [a-seq] (let [toggle (fn [a-set elem] [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
                      (reduce (fn [odd-set elem] (toggle odd-set elem)) #{} a-seq)))

(defn minus ([x] (- 0 x)) ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-*
  ([] 1)
  ([x & more] (reduce * x more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred & more] (fn [x] (reduce #(identity (and % (%2 x))) (pred x) more))))

(defn my-map
  ([f xs] (reduce (fn [acc x] (conj acc (f x))) [] xs))
  ([f xs & more]
    (loop [colls (cons xs more)
            acc []]
      (let [firsts (my-map first colls)]
        (if (some nil? firsts)
          acc
            (recur (my-map rest colls) (conj acc (apply f firsts))))))))
