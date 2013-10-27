(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (let [f (fn [a b] (concat a [x b]))]
      (reduce f (seq [(first a-seq)]) (rest a-seq)))))

(defn my-count [a-seq]
  (let [f (fn [a b]
            (inc a))]
    (reduce f 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  (let [f (fn [a b]
            (cond
              (and ( > (a 0) b) (< (a 1) b)) [b b]
              (< (a 1) b) (assoc a 1 b)
              (> (a 0) b) (assoc a 0 b)
              :else a))]
    (reduce f [(first a-seq) (first a-seq)] (rest a-seq))))


(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (seq [n])
    (cond
      (> (first sorted-seq) n) (cons n sorted-seq)
      :else (cons (first sorted-seq) (insert (rest sorted-seq) n)))))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a b]
                 (if (contains? a b)
                   (disj a b)
                   (conj a b)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([& more]
    (let [f (fn [a b]
            (inc a))]
    (reduce f 0 more))))

(defn my-*
  ([& more]
    (if (empty? more)
      1)))

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
