(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (let [with-spaces (interpose " " a-seq)]
    (reduce str with-spaces)))

(defn my-interpose [x a-seq]
  (let [a-seq-len (count a-seq)]
    (reduce (fn [acc-vec el]
              ;; acc-vec is a vector, so count is O(1)
              (if (< (count acc-vec) (- (* 2 a-seq-len) 2))
                (conj acc-vec el x)
                (conj acc-vec el)))
            []
            a-seq)))

(defn my-count [a-seq]
  (reduce (fn [cnt _] (inc cnt)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[mn mx] el] [(min mn el) (max mx el)])
          [(first a-seq) (first a-seq)]
          a-seq))

(defn insert [sorted-seq n]
  (let [smaller-or-equal (take-while #(<= % n) sorted-seq)
        greater (drop-while #(<= % n) sorted-seq)]
    (concat smaller-or-equal [n] greater)))

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

(defn count-params [& params]
  (count params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (constantly true))
  ([pred] pred)
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more] (reduce pred-and (pred-and pred1 pred2) more)))

(defn my-map
  [f & seqs]
  (if (some empty? seqs)
    []
    (let [first-elems (fn [seqs] (reduce (fn [acc s] (conj acc (first s))) [] seqs))
          rest-elems (fn [seqs] (reduce (fn [acc s] (conj acc (rest s))) [] seqs))]
      (cons
       (apply f (first-elems seqs))
       (apply my-map f (rest-elems seqs))))))
