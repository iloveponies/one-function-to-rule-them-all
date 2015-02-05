(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
    (reduce (fn [a b]
              (if (empty? a)
                (str a b)
                (str a " " b))) "" a-seq))

(defn my-interpose [x a-seq]
  (reduce (fn [a b]
            (if (empty? a)
              (conj a b)
              (conj a x b)))
          [] a-seq))

(defn my-count [a-seq]
  (reduce (fn [a b] (+ a 1)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (conj a b)) '() a-seq))

;(defn min-max-element [a-seq]
;  [(apply min a-seq) (apply max a-seq)])

(defn min-max-element [a-seq]
  (reduce (fn [a b]
            (if (empty? a)
              [b b]
              [(min (first a) b)
               (max (second a) b)]))
          [] a-seq))

(defn insert [sorted-seq n]
  (loop [ret-seq []
         a-seq sorted-seq]
    (cond
      (empty? a-seq) (conj ret-seq n)
      (>= (first a-seq) n) (concat ret-seq [n] a-seq)
      :else (recur (conj ret-seq (first a-seq)) (rest a-seq)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (reduce (fn [a b] (inc a)) 0 params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & more] (reduce * x more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] p))
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more] (reduce pred-and (pred-and p q) more)))

(defn my-map [f a-seq]
  [:-])
