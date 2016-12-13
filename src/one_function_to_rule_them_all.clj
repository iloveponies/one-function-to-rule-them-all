(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (let [hd (first a-seq)
          tl (rest a-seq)]
      (reduce #(conj %1 x %2) [hd] tl))))

(defn my-count [a-seq]
  (let [contar (fn [acc whatever]
                 (inc acc))]
    (reduce contar 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [hd (first a-seq)
          tl (rest a-seq)
          go (fn [[minn maxx] x]
               [(min minn x) (max maxx x)])]
      (reduce go [hd hd] tl))))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
    (let [hd (first sorted-seq)
          tl (rest sorted-seq)]
      (if (>= hd n)
        (cons n sorted-seq)
        (cons hd (insert tl n))))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn toggle [a-set x]
  (if (contains? a-set x)
    (disj a-set x)
    (conj a-set x)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & rst] (* (my-* x y) (reduce my-* rst))))

(defn pred-and
  ([] (fn [x] true))
  ([x] (fn [y] (x y)))
  ([x y] (fn [z] (and (x z) (y z))))
  ([x y & zs] (fn [z] (and (x z) (y z) ((reduce pred-and zs) z)))))

(defn my-map
  [f & xs]
  (if (every? empty? xs)
    []
    (let [hs (map first xs)
          tls (map rest xs)]
      (cons (apply f hs) (apply my-map f tls)))))
