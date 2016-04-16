(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
  (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reverse (reduce #(conj %1 x %2) () (seq a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [x _] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(conj %1 %2) () (seq a-seq)))



(defn min-max-element [a-seq]
  (let [fst (first a-seq)]
  (reduce
    (fn [list b] (cond
                  (< b (first list)) [b (second list)]
                  (> b (second list)) [(first list) b]
                  :else list)) [fst fst] (rest a-seq))))


(defn insert [sorted-seq n]
  (let [inserted-n #(concat (conj %1 n) %2)]
  (loop [a-vec []
          sorted-seq sorted-seq]
    (cond
      (empty? sorted-seq) (inserted-n a-vec sorted-seq)
      (< n (first sorted-seq)) (inserted-n a-vec sorted-seq)
      :else (recur (conj a-vec (first sorted-seq)) (rest sorted-seq))))))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (set
    (map first
      (filter #(not= 0 (mod (second %) 2)) (seq (frequencies a-seq))))))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (* x y (reduce * 1 more))))

(defn pred-and
  ([] (fn [x] true))
  ([x] (fn [a-seq] (x a-seq)))
  ([x & more] #(reduce (fn [a b] (and a (b %))) (x %) more)))

(defn my-map [f col]
  [:-])
