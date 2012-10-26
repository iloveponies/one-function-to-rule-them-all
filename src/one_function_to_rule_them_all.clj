(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (let [space-cat (fn [str1 str2] (str str1 " " str2))]
    (if (empty? a-seq)
      ""
      (reduce space-cat a-seq))))

(defn my-interpose [x a-seq]
  (let [do-interpose (fn [a1 a2] (conj a1 x a2))]
    (if (empty? a-seq)
      []
      (reduce do-interpose [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [count e] (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverse-join (fn [arr e] (concat [e] arr))]
    (reduce reverse-join [] a-seq)))

(defn min-max-element [a-seq]
  (let [min-max (fn [mmvec e] [(min (first mmvec) e) (max (get mmvec 1) e)])]
    (reduce min-max [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (loop [new-seq []
         old-seq sorted-seq]
    (cond
      (empty? old-seq) (conj new-seq n)
      (<= n (first old-seq)) (concat new-seq [n] old-seq)
      :else (recur (conj new-seq (first old-seq)) (rest old-seq)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [parity-count (fn [pairs e] (if (contains? pairs e) (disj pairs e) (conj pairs e)))]
    (reduce parity-count #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (my-* (reduce my-* more) y)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more] (fn [x] (and ((pred-and p q) x) ((reduce pred-and more) x)))))

(defn my-map [f & seqs]
  (loop [out []
         in seqs]
    (if (or (empty? in) (some empty? in))
      out
      (recur (conj out (apply f (map first in))) (map rest in)))))

