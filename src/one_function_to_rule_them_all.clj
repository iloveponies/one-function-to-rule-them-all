(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (sequence (reduce #(conj %1 x %2) (vector (first a-seq)) (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn[n elem]
                 (inc n))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [add-before (fn[a-seq elem]
            (cons elem a-seq))]
    (reduce add-before '() a-seq)))

(defn min-max-element [a-seq]
  (let [update-min-max (fn[[mn mx] current]
                         [(min mn current) (max mx current)])
        fst (first a-seq)]
    (reduce update-min-max [fst fst] (rest a-seq))))

(defn insert [sorted-seq n]
  (concat
    (filter #(< % n) sorted-seq)
    (list n)
    (filter #(>= % n) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn[a-set elem]
                 (if (a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([]      (fn[v] true))
  ([x?]    x?)
  ([x? y?] (fn[v] (and (x? v) (y? v))))
  ([x? y? & more?] (fn[v] (reduce #(and %1 (%2 v)) ((pred-and x? y?) v) more?))))

(filter (pred-and) [1 0 -2])                    ;=> (1 0 -2)
(filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> (1 7)
(filter (pred-and number? integer? pos? even?)
        [1 0 -2 :a 7 "a" 2])                    ;=> (0 2)

(defn my-map [f a-seq]
  [:-])
