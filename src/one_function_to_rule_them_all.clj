(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [i j] (str i " " j)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reverse (reduce (fn [i j] (if (empty? i) (conj i j) (conj i x j))) '() a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[mi ma] i] (vec (conj '() (if (nil? ma) i (max ma i)) (if (nil? mi) i (min mi i))))) [nil nil] a-seq))

(defn insert [sorted-seq n]
  (let [head (take-while (fn [i] (<= i n)) sorted-seq)
        tail (drop-while (fn [i] (<= i n)) sorted-seq)]
    (concat head (conj tail n))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [elems elem]
                 (if (contains? elems elem)
                   (disj elems elem)
                   (conj elems elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x & more] (reduce (fn [n i] (+ n 1)) 1 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & more] (reduce (fn [i j] (* i j)) x more))
  )

(defn pred-and
  ([] (fn [i] true))
  ([x] x)
  ([x & more] (fn [i] (reduce (fn [res pred] (and res (pred i))) (x i) more))))

(defn lines [res a-seq]
  (let [heads (map first a-seq)
        tails (map rest a-seq)]
    (if (nil? (first heads))
      res
      (lines (concat res (conj '() heads)) tails))))

(defn my-map
  ([f a-seq] (reduce (fn [r e] (concat r (conj '() (f e)))) '() a-seq))
  ([f a-seq & more] (reduce (fn [r e] (concat r (conj '() (apply f e)))) '() (lines '() (concat (conj '() a-seq) more)))))
