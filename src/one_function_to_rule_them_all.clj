(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat `() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b]
            (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [a b]
            (conj a x b))
          []
          a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [count elem]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [in]
  (let [rev (fn [out nxt]
              (conj out nxt))]
  (reduce rev `() in)))

(defn min-max-element [a-seq]
  (conj [] (apply min a-seq) (apply max a-seq)))

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) (concat (list n) sorted-seq)
    (> (first sorted-seq) n) (concat (list n) sorted-seq)
    :else (concat (list (first sorted-seq)) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert `() a-seq))

(defn parity [a-seq]
  (set (map first (filter (fn [x] (= 1 (mod (second x) 2))) (frequencies a-seq)))))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([& args] (+ 1 (apply count-params (rest args)))))

(defn my-*
  ([] 1)
  ([a] a)
  ([a b] (* a b))
  ([a b & more] (reduce * (my-* a b) more)))

(defn pred-and
  ([] (fn [x] true))
  ([a] (fn [x] (a x)))
  ([a & more] (fn [x]
                (and
                  (a x)
                  (apply (apply pred-and more) (list x))))))

(defn my-map [f a-seq]
  [:-])
