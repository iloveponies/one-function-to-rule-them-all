(ns one-function-to-rule-them-all)

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [a b] (conj a x b)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (cons b a)) [] a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [a b] (vector (min (first a) b)
                            (max (second a) b)))
          [9999 -9999]
          a-seq))

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) [n]
    (< n (first sorted-seq)) (cons n sorted-seq)
    :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [a b] (toggle a b)) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& more]
  (cond
    (empty? more) 1
    (= 1 (count more)) (first more)
    :else (reduce * more)))

(defn pred-and [& more]
  (cond
    (empty? more) (fn [k] true)
    (= 1 (count more)) (first more)
    :else (fn [k] (reduce (fn [a b] (and a (b k))) true more))))

(defn my-map [f a-seq & more]
  (if (empty? more)
    (if (empty? a-seq)
      []
      (cons (f (first a-seq))
            (my-map f (rest a-seq))))
    (let [all (cons a-seq more)
          a-first (my-map first all)
          a-rest (my-map rest all)]
      (if (some nil? a-first)
        []
        (cons (apply f a-first)
              (apply my-map (cons f a-rest)))))))
