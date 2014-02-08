(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc s] (str acc " " s)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce (fn [acc e] (if (empty? acc) (conj acc e) (conj acc x e))) [] a-seq))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc e] (cons e acc)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [acc e] [(min (first acc) e) (max (last acc) e)]) [10000000 -10000000] a-seq))

(defn insert [sorted-seq n]
  (cond 
    (empty? sorted-seq) [n]
    :else
    (loop [beginning []
           last-elem-of-beginning nil
           remaining sorted-seq]
      (cond 
        (empty? remaining) (conj beginning n)
        (and 
          (or (nil? last-elem-of-beginning) (<= last-elem-of-beginning n)) 
          (>= (first remaining) n)) 
        (concat (conj beginning n) remaining)
        :else
        (recur (conj beginning (first remaining)) (first remaining) (rest remaining))))))

(defn insertion-sort [a-seq]
  (reduce (fn [acc e] (insert acc e)) [] a-seq))

(defn- toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (reduce (fn [s e] (toggle s e)) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (reduce (fn [acc _] (inc acc)) 0 params))

(defn my-* [& terms]
   (reduce * terms))

(defn pred-and [& preds]
  (fn [x] (reduce (fn [acc p] (and acc (p x))) true preds)))

(defn- firsts [seqs]
  (loop [acc []
         s seqs]
    (if (empty? s)
      acc
      (recur (conj acc (ffirst s)) (rest s)))))

(defn- rests [seqs]
  (loop [acc []
         s seqs]
    (if (empty? s)
      acc
      (recur (conj acc (rest (first s))) (rest s)))))

(defn my-map [f & seqs]
  (loop [acc []
         s seqs]
    (if (some empty? s)
      acc
      (recur (conj acc (apply f (firsts s))) (rests s)))))
                                 