(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce
    (fn [acc elem] (if (empty? acc) (conj acc elem) (conj acc x elem)))
    []
    a-seq))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (reduce
    (fn [acc elem]
      (if (empty? acc)
        [elem elem]
        [(min elem (first acc)) (max elem (second acc))]))
    []
    a-seq))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
    (loop [res [] remaining sorted-seq]
      (cond
        (empty? remaining) (conj res n)
        (>= (first remaining) n) (concat res [n (first remaining)] (rest remaining))
        :else (recur (conj res (first remaining)) (rest remaining))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [acc elem] (if (contains? acc elem) (disj acc elem) (conj acc elem))) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-* [& args]
  (reduce * 1 args))

(defn pred-and
  ([] (fn [val] true))
  ([& preds] (fn [val] (every? true? (map (fn [pred] (pred val)) preds)))))

(defn my-map [f & seqs]
  (let [get-nth (fn [seqs n] (reduce (fn [x y] (concat x [(get y n)])) () seqs))
        seq-count (count (first seqs))]
    (reduce
      (fn [acc n] (concat acc [(apply f (get-nth seqs n))]))
      '()
      (range seq-count))))

(my-map inc [1 2 3 4])                  ;=> (2 3 4 5)
(my-map + [1 1 1] [1 1 1] [1 1 1])      ;=> (3 3 3)
(my-map vector [1 2 3] [1 2 3] [1 2 3]) ;=> ((1 1 1) (2 2 2) (3 3 3))
