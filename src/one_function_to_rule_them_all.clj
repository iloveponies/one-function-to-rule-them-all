(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (apply str (reduce (fn [a b] (str a " " b)) a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [a b]  (conj a x b)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [cnt _] (inc cnt)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [[n x] [(first a-seq) (first a-seq)]
        min-max (fn [pair current]
                  [(min current (get pair 0)) (max current (get pair 1))])]
    (reduce min-max [n x] a-seq)))

(defn insert [sorted-seq n]
  (loop [head []
         tail sorted-seq]
    (cond
      (empty? tail) (conj head n)
      (< n (first tail)) (concat (conj head n) tail)
      :else (recur (conj head (first tail)) (rest tail)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (minus 0 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (my-count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [_] true))
  ([p1] p1)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn zip [seqs]
  (apply map vector seqs))

(defn my-map [f & seqs]
  (reduce (fn [result z-seq] (conj result (apply f z-seq))) [] (zip seqs)))