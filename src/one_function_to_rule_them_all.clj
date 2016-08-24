(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (let [stringer (fn [x y]
                   (str x " " y))]
    (if (empty? a-seq)
      ""
      (reduce stringer a-seq))))

(defn my-interpose [x a-seq]
  (let [interposer (fn [a b]
                     (conj a x b))]
    (rest (reduce interposer [] a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [acc _]
                  (inc acc))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [min-maxer (fn [acc x]
                    (let [minimum (min (first acc) x)
                          maximum (max (second acc) x)]
                      [minimum maximum]))]
    (reduce min-maxer [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (let [smaller-than-n (fn [x] (< x n))
        start (take-while smaller-than-n sorted-seq)
        end (drop-while smaller-than-n sorted-seq)]
    (concat start (cons n end))))

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