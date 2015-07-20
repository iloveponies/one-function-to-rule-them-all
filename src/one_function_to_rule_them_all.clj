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
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ; Let's not use built-in (-) for sports!
  ([x] (* -1 x))
  ([subtractor & subtractees] (reduce + subtractor (map minus subtractees))))

(defn count-params [& params]
  (count params))

(defn my-* [& numbers]
  (reduce * 1 numbers))

(defn pred-and [& predicates]
  (fn [x] (reduce (fn [so-far predicate] (and so-far (predicate x))) true predicates)))
; (fn [v] (and (pred1 v) (pred2 v)))

(defn my-map
  ([f a-seq]
    (loop [head []
           tail a-seq]
      (if (empty? tail)
        head
        (recur (conj head (f (first tail))) (rest tail)))))
  ([f a-seq & rest-of-seqs]
    (loop [head []
           tails (cons a-seq rest-of-seqs)]
      (if (some empty? tails)
        head
        (recur (conj head (apply f (my-map first tails))) (my-map rest tails))))))
