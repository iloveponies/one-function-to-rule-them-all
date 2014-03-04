(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    ; this is equivalent to manually passing first element
    ; (reduce (fn [acc, y] (str acc " " y)) (first a-seq) (rest a-seq))
    (reduce (fn [acc, y] (str acc " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    ; naming with acc for accumulator helps
    ; knows to pass last element
    (reduce (fn [acc, el] (conj acc x el)) (vector (first a-seq)) (rest a-seq))))

(defn my-count [a-seq]
  ; knows to pass last element, function name just used
  (let [acc-fn (fn [acc, el]
                 (inc acc))]
    (reduce acc-fn 0 a-seq)))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [acc-fn (fn [acc, el]
                   (cons el acc))]
      (reduce acc-fn (vector (first a-seq)) (rest a-seq)))))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [find-max-min-vector (fn [max-min-vector, el]
                   [(min (first max-min-vector) el) (max (second max-min-vector) el)])]
      (reduce find-max-min-vector [(first a-seq) (first a-seq)] a-seq))))

(defn insert [sorted-seq n]
  [:-])

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