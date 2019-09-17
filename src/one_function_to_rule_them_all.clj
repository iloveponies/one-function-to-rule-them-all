(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (reduce str "" (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (seq (reduce (fn [first second]
                 (if (empty? first) (conj first second)
                     (conj first x second))) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [first second]
            (cons second first)) [] a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (let [place (count (filter #(> n %) sorted-seq))
        [before after] (split-at place sorted-seq)]
    (concat before (cons n after))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
  (disj a-set elem)
  (conj a-set elem)))

(defn parity [a-seq]
  (reduce (fn [a b] (toggle a b)) #{} a-seq))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& args]
  (my-count args))

(defn my-* [& args]
  (reduce * args))

(defn pred-and [& args]
  (fn [x] (reduce (fn [acc pred] (and acc (pred x))) true args)))

(defn my-map [f a-seq & seqs]
  :-)
