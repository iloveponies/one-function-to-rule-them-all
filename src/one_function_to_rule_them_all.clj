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
  (loop [acc-seq []
         current-seq sorted-seq]
    (cond
      (empty? current-seq) (conj acc-seq n)
      (< (first current-seq) n) (recur
                                  (conj acc-seq (first current-seq))
                                  (rest current-seq))
      :else
      (concat acc-seq [n] current-seq))))

(defn insertion-sort [a-seq]
  ; similar as (reduce (fn [acc, x] (insert acc x)) (vector (first a-seq)) (rest a-seq))
  (reduce (fn [acc, x] (insert acc x)) [] a-seq))

; only for adding
(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  ; same as (reduce (fn [acc, el] (toggle acc el)) #{} a-seq)
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (* -1 x))
  ([x, y] (- x y)))

(defn count-params [& more]
  (count more))

; if without using arity
;(defn my-* [& more]
;  (let [input-arguments-count (count more)]
;    (cond
;      (== input-arguments-count 0) 1
;      (== input-arguments-count 1) (first more)
;      (== input-arguments-count 2) (* (first more) (second more))
;      (> input-arguments-count 2)
;          ; same as with acc expliticly stated: (reduce * (first more) (rest more))
;          (reduce * more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred] (fn [x] (pred x)))
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more]
   (reduce pred-and (pred-and pred1 pred2) more)))

(defn my-map [f a-seq]
  [:-])