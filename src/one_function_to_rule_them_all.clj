(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [string s] (str string " " s)) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [interpose elem] (conj interpose x elem)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [count _] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [[fst & rst]]
  (let [chooser (fn [[min1 max1] x]
                  [(min min1 x) (max max1 x)])]
    (reduce chooser [fst fst] rst)))

(defn insert [sorted-seq n]
  (loop [right '()
         left sorted-seq]
    (if (or (empty? left) (< n (first left)))
      (concat right (list n) left)
      (recur (concat right (list (first left)))
             (rest left)))))


(defn insert [sorted-seq n]
  (loop [counter 0
         seq sorted-seq]
    (if (> (first seq) n)
      (concat (take n) [n] (drop n))
      (recur (inc counter)
             (rest seq)))))


(defn insertion-sort [a-seq]
  [:-])

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
