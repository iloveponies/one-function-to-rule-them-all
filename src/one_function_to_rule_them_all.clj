(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce (fn [a b]
            (if (empty? a) (conj a b) (conj a x b)))
          [] a-seq))

(defn my-count [a-seq]
  (reduce (fn [c e] (inc c)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [helper (fn [[s l] x]
                 [(min s x) (max l x)])]
    (reduce helper [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (let [lt-n (fn [x] (< x n))]
    (concat (take-while lt-n sorted-seq) (cons n '())
            (drop-while lt-n sorted-seq))))

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

(defn count-params [& args]
  (my-count args))

(defn my-* [& args]
  (reduce * 1 args))

(defn pred-and [& preds]
  (fn [x] (reduce (fn [p f] (and p (f x))) true preds)))

(defn my-map [f a-seq]
  [:-])

; (O _ o)
