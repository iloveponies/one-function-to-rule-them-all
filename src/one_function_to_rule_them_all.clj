(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (reduce (fn [a b] (conj a x b)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc x] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [acc x] (conj acc x)) '() a-seq)))

(defn min-max-element [a-seq]
  (reduce (fn [[a-min a-max] x]
            [(min a-min x) (max a-max x)]) [(first a-seq) (first a-seq)] a-seq))


(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) (list n)
    (> (first sorted-seq) n) (cons n sorted-seq)
    :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set a-key]
  (if (contains? a-set a-key)
    (disj a-set a-key)
    (conj a-set a-key)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& xs]
  (reduce * 1 xs))

(defn pred-and
  ([& more]
   (fn [x] (reduce (fn [acc, pred] (and acc (pred x))) true more))))

(defn my-zip [& seqs]
  (if (some empty? seqs)
    '()
    (cons
     (reverse (reduce (fn [acc a-seq] (cons (first a-seq) acc)) '() seqs))
     '(());(my-zip (reverse (reduce (fn [acc a-seq] (cons (rest a-seq) acc)) '() seqs ))))))


(defn my-map [f & seqs]
  (if (some empty? seqs)
    '()
    (cons
     (apply f (reverse (reduce (fn [acc a-seq] (cons (first a-seq) acc)) '() seqs)))
     (my-map f (reverse (reduce (fn [acc a-seq] (cons (rest a-seq) acc)) '() seqs ))))))
