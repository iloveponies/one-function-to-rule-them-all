(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (loop [a-seq a-seq
         con-seq []]
    (if (empty? a-seq)
      con-seq
      (recur (rest a-seq) (concat con-seq (first a-seq))))))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce (fn [a b] (if (empty? a) (conj [] b) (conj a x b))) [] a-seq))

(defn my-count [a-seq]
  (reduce (fn [count e] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (cons b a)) () a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [a b] [(apply min (conj a b)) (apply max (conj a b))]) [] a-seq))

(defn insert [sorted-seq n]
  (loop [sorted-seq sorted-seq
         n n
         acc-seq ()]
    (cond
      (empty? sorted-seq)      (conj acc-seq n)
      (> (first sorted-seq) n) (concat (conj (vec acc-seq) n) sorted-seq)
      :else                    (recur (rest sorted-seq) n (conj (vec acc-seq) (first sorted-seq))))))

(defn insertion-sort [a-seq]
  (reduce (fn [a b] (insert a b)) [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& args]
  (reduce (fn [count e] (inc count)) 0 args))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([& more] (fn [x] (every? (fn [p] (p x)) more))))

(defn my-map [f a-seq]
  [:-])
