(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc elem] (str acc " " elem)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (rest (reduce (fn [acc elem] (conj acc x elem)) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc elem] (cons elem acc)) () a-seq))

(defn min-max-element [a-seq]
  (let [min-max-helper (fn [acc elem]
                         (if (empty? acc)
                           (vector elem, elem)
                           (vector (min (first acc) elem) (max (second acc) elem))))]
  (reduce min-max-helper [] a-seq))) ;[] is empty vector

(defn insert [sorted-seq n]
  (loop [s sorted-seq
         acc [n]]
    (if (= 0 (count s))
      acc
      (let [new-acc
            (if (< n (first s))
              (conj acc (first s))
              (conj (vec (butlast acc)) (first s) n))] ;vec drops nil, vector to make conj append instead of prepend
      (recur (rest s) new-acc)))))


(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn my-toggle [a-seq elem]
  (if (contains? a-seq elem)
    (disj a-seq elem)
    (conj a-seq elem)))

(defn parity [a-seq]
  (reduce my-toggle #{} a-seq));#{} apparently empty set


(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))


(defn count-params
  ([] 0)
  ([x] 1)
  ([x y] 2)
  ([x y & more]
    (+ 2 (count more))))

;this works too
(defn count-params-alternative
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))


;from predicates ex
;(defn pred-and [pred1 pred2]
;  (fn [x] (and (pred1 x) (pred2 x))))


;    If no parameters are given, return a predicate that always returns true.
;    If only one predicate p is given, return p.
;    If two predicates are given, return a predicate that returns true if both of them return true and false otherwise.
;    If more than two predicates are given, return a predicate that returns true only if all of the predicates return true and false otherwise.
(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
   (reduce pred-and (fn [x] (and (p1 x) (p2 x))) more)))


;(defn my-map [f seq-1 seq-2]
;  (if (and (empty? seq-2) (empty? seq-2))
;     ()
;     (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))
; EXTRA
(defn my-map [f a-seq]
  [:-])
