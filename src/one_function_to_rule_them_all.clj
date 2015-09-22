(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [cur nxt] (str cur " " nxt)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (reduce (fn [cur nxt] (conj cur x nxt)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [cur nxt] (+ 1 cur)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [cur nxt] (cons nxt cur)) () a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [cur nxt] [(min (first cur) nxt) (max (last cur) nxt)]) [(first a-seq) (last a-seq)] (rest a-seq)))

(defn insert [sorted-seq n]
  (let [split (split-with (fn [x] (<= x n)) sorted-seq)]
    (concat (first split) [n] (last split))))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (letfn [(toggle [a b]
                  (if (contains? a b) (disj a b) (conj a b)))]
    (reduce (fn [cur nxt] (toggle cur nxt)) #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map [f a-seq]
  [:-])