(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) "" (reduce #(str % " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce #(conj % x %2) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [s _](inc s)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %) '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq) []
  (let [f (first a-seq)]
    (reduce #(identity [(min (% 0) %2) (max (% 1) %2)]) [f f] (rest a-seq)))))

(defn insert [sorted-seq n]
  (loop [so-far [] xs sorted-seq]
    (let [fxs (first xs)]
      (cond (nil? fxs) (conj so-far n)
            (> fxs n) (concat (conj so-far n) xs)
            :else (recur (conj so-far fxs) (rest xs))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set element]
  ((partial (if (contains? a-set element) disj conj)) a-set element))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-*
  ([] 1)
  ([x & more] (reduce * x more)))


(defn pred-and
  ([] (fn [x] true))
  ([pred & more] (fn [x] (reduce #(identity (and % (%2 x))) (pred x) more))))

(defn my-map
  ([f xs] (reduce (fn [acc x] (conj acc (f x))) [] xs))
  ([f xs & more]
   (loop [colls (cons xs more)
          acc []]
     (let [firsts (my-map first colls)]
       (if (some nil? firsts)
         acc
         (recur (my-map rest colls) (conj acc (apply f firsts))))))))




