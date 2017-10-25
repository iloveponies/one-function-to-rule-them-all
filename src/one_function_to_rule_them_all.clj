(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [str1 str2] (str str1 " " str2)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [vec elem] (conj vec x elem)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [count _] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  (let [first (first a-seq)
        min-max (fn [[mini maxi] x] [(min mini x) (max maxi x)])]
    (reduce min-max [first first] (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [result []
        remaining sorted-seq]
    (cond
      (empty? remaining) (concat result [n])
      (< n (first remaining)) (concat result (cons n remaining))
      :else (recur (conj result (first remaining)) (rest remaining)))))

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

(defn count-params [& args]
  (count args))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

  (defn pred-and
    ([] (fn [_] true))
    ([p?] p?)
    ([p1? p2?] (fn [x] (and (p1? x) (p2? x))))
    ([p1? p2? & more] (reduce pred-and (pred-and p1? p2?) more)))

(defn my-map [f a-seq]
  [:-])