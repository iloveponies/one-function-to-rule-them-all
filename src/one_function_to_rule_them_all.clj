(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x k] (str x " " k)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [k c] (conj k x c)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [acc k] (inc acc))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [k c] (cons c k))]
    (reduce reverser '() a-seq)))

(defn min-max-element [a-seq]
  (let [comparing-function (fn [[a b] x]
                             (cond
                               (> x b) [a x]
                               (< x a) [x b]
                               :else   [a b]))]
    (reduce comparing-function [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
    (loop [left-part '()
           right-part sorted-seq]
       (if (or (empty? right-part) (<= n (first right-part)))
               (concat left-part (cons n '()) right-part)
               (recur (concat left-part (cons (first right-part) '())) (rest right-part)))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& a-seq]
  (count a-seq))

(defn my-* [& a-seq]
  (let [number (count a-seq)]
    (cond
      (= number 0) 1
      (= number 1) (first a-seq)
      :else        (reduce * a-seq))))

(defn pred-and
  ([]  (fn [x] true))
  ([x] x)
  ([x y] (fn [c] (and (x c) (y c))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn my-map [f a-seq]
  [:-])
