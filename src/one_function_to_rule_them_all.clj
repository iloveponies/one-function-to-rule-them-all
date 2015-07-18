(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  ; Seems funny.
  (reduce str "" (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [agg cur] (conj agg x cur)) [(first a-seq)] (rest a-seq))))
;  (loop [aggregate []
;         a-seq a-seq]
;    (if (empty? a-seq)
;      (rest aggregate)
;      (recur (conj (conj aggregate x) (first a-seq)) (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [n _] (inc n)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [first-value (first a-seq)
        min-max-scan (fn [[minimum maximum] current]
                       [(min minimum current) (max maximum current)])]
    (reduce min-max-scan [first-value first-value] a-seq)))

(defn insert [sorted-seq n]
  (loop [head []
         tail sorted-seq]
    (let [first-of-tail (first tail)]
      (if (or (empty? tail) (<= n first-of-tail))
        (concat (conj head n) tail)
        (recur (conj head first-of-tail) (rest tail))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

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
