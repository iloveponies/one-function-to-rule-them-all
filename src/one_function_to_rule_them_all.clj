(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [a b] (conj a x b)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [c e] (inc c)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (conj a b)) '() a-seq))

(defn min-max-element [a-seq]
  (let [min-max (fn [[curr-min curr-max]e]
                    [(min curr-min e)
                     (max curr-max e)])
        head    (first a-seq)
        tail    (rest a-seq)]
    (reduce min-max [head head] tail)))

(defn insert [sorted-seq n]
  (loop [sorted-seq sorted-seq
         curr       []]
        (cond
          (empty? sorted-seq)      (conj curr n)
          (> (first sorted-seq) n) (concat (conj curr n)
                                           sorted-seq)
          :else                    (recur (rest sorted-seq)
                                          (conj curr (first sorted-seq))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set k]
                   (if (contains? a-set k)
                     (disj a-set k)
                     (conj a-set k)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn pred-and
  ([& more] (reduce (fn [x y] (fn [z]
                                  (and (x z)
                                       (y z))))
                    (fn [x] true)
                    more)))

(defn my-map
  ([f & more]
   (loop [curr []
          more more]
         (if (some empty? more)
           curr
           (let [all-first (reduce (fn [x y]
                                       (if (empty? y)
                                         []
                                         (conj x (first y))))
                                   []
                                   more)
                 all-rest  (reduce (fn [x y]
                                       (conj x (rest y)))
                                   []
                                   more)]
             (recur (conj curr (apply f all-first))
                    all-rest))))))
