(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc next] (str acc " " next)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [acc next] (conj acc x next)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc next] (cons next acc)) '() a-seq))

(defn min-max-element [a-seq]
  (let [f (first a-seq)
        helper (fn [[min max] next]
                 (cond
                   (< next min) [next max]
                   (> next max) [min next]
                   :else [min max]))]
    (reduce helper [f f] (rest a-seq))))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
    (let [f (first sorted-seq)]
      (if (< n f)
        (cons n sorted-seq)
        (cons f (insert (rest sorted-seq) n))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set element]
                 (if (contains? a-set element)
                   (disj a-set element)
                   (conj a-set element)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [_] true))
  ([p] p)
  ([p & more] (fn [x] (reduce (fn [current next] (and current (next x))) (p x) more))))

(defn my-map
  ([f a-seq]
     (let [helper (fn [acc next] (conj acc (f next)))]
       (reduce helper [] a-seq)))
  ([f a-seq & more] (if (empty? a-seq)
                      []
                      (let [firsts (my-map first (cons a-seq more))
                            rests (my-map rest (cons a-seq more))]
                        (cons (apply f firsts) (apply my-map f rests))))))
