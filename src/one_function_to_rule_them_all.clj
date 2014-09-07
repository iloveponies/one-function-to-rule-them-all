(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (cond
   (empty? a-seq) []
   (empty? (next a-seq)) a-seq
   :else
   (reduce #(conj %1 x %2) (vector (first a-seq)) (rest a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [a-count elem]
                  (inc a-count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [min-max-reducer (fn [min-max elem]
                            (vector
                             (min (first min-max) elem)
                             (max (last min-max) elem)))]
      (reduce min-max-reducer [(first a-seq) (first a-seq)] (rest a-seq)))))

(defn insert [sorted-seq n]
  (loop [result-seq '()
         curr-seq sorted-seq]
    (let [fst-elem (first curr-seq)]
      (if (or (empty? curr-seq)
              (<= n fst-elem))
        (concat result-seq (cons n curr-seq))
        (recur (concat result-seq (list fst-elem)) (rest curr-seq))))))

(defn insertion-sort [a-seq]
  (reduce #(insert %1 %2) [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set
                   elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (let [count-fn (fn [x y] (inc x))]
              (reduce count-fn 0 more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] #(= %))
  ([x] x)
  ([x y] #(and (x %) (y %)))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn my-map [f a-seq]
  [:-])
