(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [acc v] (if (empty? acc) (conj acc v) (conj acc x v))) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc v] (cons v acc)) '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    nil
    (reduce (fn [[acc-min acc-max] v] [(min v acc-min) (max v acc-max)]) [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [acc []
         l-seq sorted-seq]
    (let [first (first l-seq)]
      (cond
        (empty? l-seq) (conj acc n)
        (<= n first) (concat acc [n] l-seq)
        :else (recur (conj acc first) (rest l-seq))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [as a] (if (contains? as a) (disj as a) (conj as a)))]
    (if (empty? a-seq)
      #{}
      (reduce toggle #{} a-seq))))

(defn minus ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& xs]
  (reduce * 1 xs))

(defn pred-and [& more]
  (fn [x] (loop [acc true
                 pred-seq more]
            (cond
              (empty? pred-seq) acc
              (false? acc) false
              :else (recur ((first pred-seq) x) (rest pred-seq))))))

(defn my-map [f a-seq & more]
  (let [any-empty? (fn [xs] (reduce (fn [acc v] (or acc (empty? v))) false xs))
        firsts (fn [xs] (reduce (fn [acc x] (conj acc (first x))) [] xs))
        rests (fn [xs] (reduce (fn [acc x] (conj acc (rest x))) [] xs))]
    (loop [acc []
           l-seqs (cons a-seq more)]
      (if (any-empty? l-seqs)
        acc
        (recur (conj acc (apply f (firsts l-seqs))) (rests l-seqs))))))