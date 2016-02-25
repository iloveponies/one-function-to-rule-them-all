(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (reduce (fn [s a] (concat s [x a])) (take 1 a-seq) (rest a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev (fn [f e]
              (cons e f))]
  (reduce rev [] a-seq)))

(defn min-max-element [a-seq]
  (let [min-max (fn [[my-min my-max] e]
                  [(min my-min e) (max my-max e)])]
    (reduce min-max [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (let [[start end] (split-with (partial > n) sorted-seq)]
    (concat start [n] end)))

(defn insertion-sort [a-seq]
    (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [p (fn [s e]
            (if (contains? s e) (disj s e) (conj s e)))]
    (reduce p #{} a-seq)))

(defn minus
  ([a] (- a))
  ([a b] (- a b)))

(defn count-params [& args]
  (reduce (fn [a e] (inc a)) 0 args))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & rest] (reduce * (* x y) rest)))

(defn pred-and
  ([] (fn [k] true))
  ([x] x)
  ([x y] (fn [k] (and (x k) (y k))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn my-map [f & seqs]
  (let [r (fn [s x] (conj s (apply f x)))
        c (count seqs)
        s (partition c (if (= c 1) (first seqs) (apply interleave seqs)))]
    (reduce r [] s)))
