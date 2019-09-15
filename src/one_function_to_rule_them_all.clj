(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj % x %2) [] a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [count e] (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (into () a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (concat (take-while #(< % n) sorted-seq)
          (cons n (drop-while #(< % n) sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (into #{} (keys (filter #(odd? (val %)) (frequencies a-seq)))))

(defn minus
  ([x] (* x -1))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [p] true))
  ([& more] (reduce (fn [p q] #(and (p %) (q %))) (first more) (rest more))))

(defn firsts [coll]
  (cond
    (empty? coll) '()
    :else (cons (ffirst coll) (firsts (rest coll)))))

(defn rests [coll]
  (cond
    (empty? coll) '()
    :else (cons (rest (first coll)) (rests (rest coll)))))

(defn my-map-helper [f s acc]
  (if (some empty? s)
    acc
    (let [x (firsts s)]
      (recur f (rests s) (conj acc (apply f x))))))

(defn my-map [f & a-seq]
  (my-map-helper f a-seq []))