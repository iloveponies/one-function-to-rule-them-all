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
  [:-])

(defn minus [x]
  :-)

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
