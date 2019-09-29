(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [a b]
              (if (= b (last a-seq))
                (conj a b)
                (conj a b x)))
            []
            a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [x y] (inc x))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [builder (fn [initial elem]
                  (conj initial elem))]
    (reduce builder '() a-seq)))

(defn min-max-element [a-seq]
  (let [min-max (fn [initial elem]
                  (vector
                     (min elem (first initial))
                     (max elem (second initial))))]
    (reduce min-max (vector (first a-seq) (first a-seq)) a-seq)))

(defn insert [sorted-seq n]
  (loop [a-seq sorted-seq
         result []
         not-added true]
    (let [current (first a-seq)]
      (cond
       (and (not not-added)(nil? current)) result
       (and not-added (or (nil? current)(<= n current)))
         (recur a-seq (conj result n) false)
       :else (recur (rest a-seq) (conj result current) not-added)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set x]
                (if (contains? a-set x)
                  (disj a-set x)
                  (conj a-set x)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-* [& params]
  (reduce * 1 params))

(defn pred-and [& params]
  (fn [x]
    (every? #(% x) params)))

(defn apply-all [func more acc]
  (if (empty? more)
    (reverse acc)
    (recur func (rest more) (cons (func (first more)) acc))))

(defn my-map
  ([func a-seq] (if (empty? a-seq)
                  '()
                  (cons
                   (func (first a-seq))
                   (my-map func (rest a-seq)))))
  ([func a-seq & sequences] (if (or (empty? a-seq) (some empty? sequences))
                      '()
                      (cons (apply func (first a-seq) (apply-all first sequences []))
                            (apply my-map func (rest a-seq) (apply-all rest sequences []))))))
