(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [acc k] (conj acc x k)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [acc n] (inc acc))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev (fn [acc n] (conj acc n))]
    (reduce rev '() a-seq)))

(defn min-max-element [a-seq]
  (let [min-max (fn [acc n] (cond
                              (< n (first acc)) (assoc acc 0 n)
                              (> n (second acc)) (assoc acc 1 n)
                              :else acc))]
    (reduce min-max [(first a-seq) (first a-seq)] a-seq)))

(defn insert-elem [a-seq i elem]
  (concat (take i a-seq) [elem] (drop i a-seq)))

(defn insert [sorted-seq n]
  (loop [sorted sorted-seq i 0]
    (cond 
      (== i (count sorted-seq)) (conj sorted n)
      (< n (get sorted i)) (apply vector (insert-elem sorted i n))
      :else (recur sorted (inc i)))))

(defn insertion-sort [a-seq]
  (reduce insert [(first a-seq)] (rest a-seq)))

(defn parity [a-seq]
  (reduce (fn [acc n] (if (contains? acc n)
                        (disj acc n)
                        (conj acc n)))
          #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-* [& x]
  (reduce * x))

(defn pred-and
  ([] (fn [x] true))
  ([f] (fn [x] (f x)))
  ([f & more] (fn [x] (reduce
                        (fn [acc func] (and acc (func x)))
                        (f x)
                        more))))

(defn apply-all [func more acc]
  (if (empty? more)
    (reverse acc)
    (recur func (rest more) (cons (func (first more)) acc))))

(defn my-map
  ([f a-seq] (if (empty? a-seq)
               '()
               (cons (f (first a-seq)) (my-map f (next a-seq)))))
  ([f a-seq & more] (if (or (empty? a-seq) (some empty? more))
                      '()
                      (cons (apply f (first a-seq) (apply-all first more []))
                            (apply my-map f (rest a-seq) (apply-all rest more []))))))
