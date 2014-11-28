(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [str1 str2] (str str1 " " str2)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [acc elm] (conj acc x elm)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [count _] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc elm] (cons elm acc)) [] a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [min-max elm]
            (cond
              (< elm (first min-max)) [elm (last min-max)]
              (> elm (last min-max)) [(first min-max) elm]
              :else min-max))
          [(first a-seq) (first a-seq)]
          (rest a-seq)))

(defn insert [sorted-seq n]
  (loop [acc [] a-seq sorted-seq]
    (cond
      (empty? a-seq) (conj acc n)
      (< n (first a-seq)) (concat (conj acc n) a-seq)
      :else (recur (conj acc (first a-seq)) (rest a-seq)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))


(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more] (reduce pred-and (pred-and p q) more)))

(defn multi-first [seqs]
  (reduce
    (fn [acc a-seq] (conj acc (first a-seq)))
    [(first (first seqs))]
    (rest seqs)))

(defn multi-rest [seqs]
  (reduce
    (fn [acc a-seq] (conj acc (rest a-seq)))
    [(rest (first seqs))]
    (rest seqs)))

(defn my-map [f & seqs]
   (loop [acc [] a-seqs seqs]
     (if (some empty? a-seqs)
       acc
       (recur
         (conj acc (apply f (multi-first a-seqs)))
         (multi-rest a-seqs)))))
