(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '[]
    (reduce #(conj (conj %1 x) %2) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [n _] (inc n)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [rev elem] (conj rev elem)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [min-max elem]
            (cond (< elem (first min-max))
                    [elem (last min-max)]
                  (< (last min-max) elem)
                    [(first min-max) elem]
                  :else
                    min-max))
          [(first a-seq) (first a-seq)]
          a-seq))

(defn insert [sorted-seq n]
    (loop [acc '()
           sorted-seq sorted-seq
           n n]
      (cond (empty? sorted-seq)
              (concat acc (list n))
            (< n (first sorted-seq))
              (concat acc (conj (apply list sorted-seq) n))
            :else
              (recur (concat acc (list (first sorted-seq))) (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [a-set elem]
            (if (contains? a-set elem)
              (disj a-set elem)
              (conj a-set elem)))
          #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))


(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([& more] (apply * more))
  )

(defn pred-and
  ([] (fn [x] true))
  ([x] (x))
  ([x y] (fn [v] (and (x v) (y v))))
  ([x y & more] (fn [v] (and (x v) ((apply pred-and y more) v)))))

(defn my-map [f & a-seq]
  (loop [acc '[]
       a-seq a-seq]
  (if (empty? (first a-seq))
    acc
    (recur (conj acc (apply f (reduce (fn [a-seq elem] (conj a-seq (first elem)))
                   '()
                   a-seq)))
           (reduce (fn [a-seq elem] (conj a-seq (rest elem)))
                   '()
                   a-seq)))))
