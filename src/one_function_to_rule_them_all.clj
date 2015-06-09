(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
  ""
  (reduce (fn [acc val] (str acc " " val)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(conj %1 %2) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [acc x] (let [[mn mx] acc] (vector (min mn x) (max mx x))))
  [(first a-seq) (first a-seq)] (rest a-seq)))

(defn insert [sorted-seq n]
  (let [elem (first sorted-seq)]
    (cond
      (empty? sorted-seq) (cons n sorted-seq)
      (<= n elem) (cons n sorted-seq)
      :else (cons elem (insert (rest sorted-seq) n)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce #(if (contains? %1 %2) (disj %1 %2) (conj %1 %2)) #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y & more] (reduce #(* %1 %2) (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred] pred)
  ([pred1 pred2 & more] (fn [x] (reduce (fn [acc pred] (and acc (pred x))) (and (pred1 x) (pred2 x)) more))))

(defn my-map
  ([f & more]
    (loop [acc []
           seqs more]
           (let [the-first (fn [a-seq] (reduce #(conj %1 (first %2)) [] a-seq))
                 the-rest (fn [a-seq] (reduce #(conj %1 (rest %2)) [] a-seq))]
                 (cond
                   (every? empty? seqs) acc
                   :else (recur (conj acc (apply f (the-first seqs))) (the-rest seqs)))))))

;(defn my-map
;  ([f & more]
;    (let [the-first (fn [a-seq] (reduce #(conj %1 (first %2)) [] a-seq))
;          the-rest (fn [a-seq] (reduce #(conj %1 (rest %2)) [] a-seq))]
;      (cond
;        (every? empty? more) []
;        :else (cons (my-map-helper f the-first(more)) (my-map f (the-rest more)))))))

; For [1 2 3] [1 2 3] [1 2 3]
; (reduce #(cons (first %2) %1) [] [[1 2 3] [1 2 3] [1 2 3]]) => (1 1 1)
; (reduce #(cons (rest %2) %1) [] [[1 2 3] [1 2 3] [1 2 3]]) => ((2 3) (2 3) (2 3))
