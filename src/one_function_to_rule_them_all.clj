(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [a b] (conj a x b)) (vector (first a-seq)) (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [c e] (inc c)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [my-list e]
            (cons e my-list)) '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (let [current (first a-seq)]
      (reduce (fn [[min-val max-val] e]
                [(min min-val e) (max max-val e)]) [current current] a-seq))))

(defn insert [sorted-seq n]
  (concat (filter #(<= % n) sorted-seq)
          (seq [n])
          (filter #(< n %) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (set (keys (filter #(odd? (val %))
                     (reduce (fn [freqs e]
                               (assoc freqs e (if (contains? freqs e)
                                                (inc (freqs e))
                                                1)))
                             {}
                             a-seq)))))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x y]
   (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([x] #(x %))
  ([x & more]
   #(reduce (fn [old c] (and old (c %))) (x %) more)))

(defn my-map [f a-seq]
  [:-])
