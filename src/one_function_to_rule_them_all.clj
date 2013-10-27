(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) ()
    (rest (reduce #(conj %1 x %2) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [x y] (+ 1 x)) 0 a-seq))

;; Reminder:
;;  (conj [] 1 2 3) ;=> (3 2 1)
;;  (conj () 1 2 3) ;=> [1 2 3]
(defn my-reverse [a-seq]
  (reduce #(conj %1 %2) () a-seq))

(defn min-max-element [a-seq]
  (let [eka (first a-seq)]
    (reduce
      (fn [[mini maxi] a]
        (let [mini (if (< a mini) a mini)
              maxi (if (> a maxi) a maxi)]
          [mini maxi])) [eka eka] a-seq)))

(defn insert [sorted-seq a]
  (let [eka (first sorted-seq)]
    (cond
      (empty? sorted-seq) (list a)
      (<= a eka) (cons a sorted-seq)
      :else (conj (insert (rest sorted-seq) a) eka))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [f (fn [a-set elem]
            (cond
              (contains? a-set elem) (disj a-set elem)
              :else (conj a-set elem)))]
    (reduce f #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (my-count more))

(defn my-*
  ([] 1)
  ([a] a)
  ([a b] (* a b))
  ([a b & more] (reduce my-* (* a b) more)))

(defn pred-and
  ([] (fn [x] true))
  ([a] (fn [x] (a x)))
  ([a b] (fn [x] (and (a x) (b x))))
  ([a b & more] (reduce pred-and a (conj more b))))

(defn my-map [f a-seq]
  [:-])
