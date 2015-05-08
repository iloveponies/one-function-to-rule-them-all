(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [i x] (str i " " x)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce (fn [acc item] (if (empty? acc)
                           (seq [item])
                           (concat acc (seq [x item]))))
          () a-seq))

(defn my-count [a-seq]
  (reduce (fn [acc item] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc item] (if (empty? acc)
                           (seq [item])
                           (concat (seq [item]) acc)))
          () a-seq))

(defn min-max-element [a-seq]
  (let [fst (first a-seq)]
    (reduce (fn [acc item] (let [[mn mx] acc]
                            [(min mn item) (max mx item)]))
            [fst fst] a-seq)))

(defn insert [sorted-seq n]
  (loop [o-seq () 
         i-seq sorted-seq]
    (if (empty? i-seq)
      (concat o-seq [n])
      (let [fst (first i-seq)]
        (if (>= fst n)
          (concat o-seq [n] i-seq)
          (recur (concat o-seq [fst]) (rest i-seq)))))))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x & more] (reduce (fn [acc arg] (inc acc)) 1 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([pa pb] (fn [x] (and (pa x) (pb x))))
  ([pa pb & more] (reduce pred-and (pred-and pa pb) more)))


(defn my-deal [acc a-seq]
  (first (reduce (fn [[nxt cur] x] [(concat nxt [(concat (first cur) [x])])
                                    (rest cur)])
                 [() acc] a-seq)))

(defn my-zip [& seqs]
  (reduce my-deal () seqs))

(defn my-map [f & seqs]
  (reduce (fn [acc x] (concat acc [(apply f x)])) () (apply my-zip seqs)))
