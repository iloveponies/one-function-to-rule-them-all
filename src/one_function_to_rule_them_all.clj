(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
    (reduce concat () a-seq))

(defn str-cat [a-seq]
    (if (empty? a-seq)
        ""
        (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
    (if (empty? a-seq)
        ()
        (reduce (fn [a b] (conj a x b))
                [(first a-seq)]
                (rest a-seq))))

(defn my-count [a-seq]
    (reduce (fn [c e] (inc c)) 0 a-seq))

(defn my-reverse [a-seq]
    (reduce (fn [x y] (cons y x)) () a-seq))

(defn min-max-element [a-seq]
    (let [helper (fn [x curr]
                     [(min (first x) curr) (max (last x) curr)])]
        (reduce helper
                [(first a-seq) (first a-seq)]
                (rest a-seq))))

(defn insert [sorted-seq n]
    (cond
        (empty? sorted-seq)            (seq [n])
        (< n (first sorted-seq))       (cons n sorted-seq)
        :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
    (reduce insert [] a-seq))

(defn parity [a-seq]
    (let [toggle (fn [a-set elem]
                     (if (contains? a-set elem)
                         (disj a-set elem)
                         (conj a-set elem)))]
        (reduce toggle #{} a-seq)))

(defn minus
    ([x]   (- x))
    ([x y] (- x y)))

(defn count-params [& more]
    (reduce (fn [c x] (inc c)) 0 more))

(defn my-* [& more]
    (reduce (fn [prod x] (* prod x)) 1 more))

(defn pred-and
    ([]             (fn [x] true))
    ([p]            (fn [x] (p x)))
    ([p1 p2]        (fn [x] (and (p1 x) (p2 x))))
    ([p1 p2 & more] (fn [x] (reduce (fn [v p] (and v (p x)))
                                    (and (p1 x) (p2 x))
                                    more))))

(defn first-of-all [more]
    (loop [seqs more
           result []]
        (if (empty? seqs)
            result
            (recur (rest seqs) (conj result (first (first seqs)))))))

(defn rest-of-all [more]
    (loop [seqs more
           result []]
        (if (empty? seqs)
            result
            (recur (rest seqs) (conj result (rest (first seqs)))))))

(defn my-map [f & more]
    (loop [seqs more
           result []]
        (if (nil? (some empty? seqs))
            (recur (rest-of-all seqs) (conj result (apply f (first-of-all seqs))))
            result)))
