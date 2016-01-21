(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (cond
   (empty? a-seq) ""
   :else (reduce (fn [acc s] (str acc " " s)) a-seq)))

(defn my-interpose [x a-seq]
  (cond
   (empty? a-seq) '()
   :else (reduce (fn [acc elem] (concat acc [x] [elem])) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc elem] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc elem] (cons elem acc)) '() a-seq))

(defn min-max-element [a-seq]
  (defn update-min-max [[mini maxi] elem]
    [(min mini elem) (max maxi elem)])
  (reduce update-min-max [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq) (cons n '())
   (< n (first sorted-seq)) (cons n sorted-seq)
   :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (* x -1))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [a-value] true))
  ([p] p)
  ([p1 p2] (fn [a-value] (and (p1 a-value) (p2 a-value))))
  ([p1 p2 & more] (fn [a-value] (reduce (fn [acc p] (and acc (p a-value))) (and (p1 a-value) (p2 a-value)) more))))

(defn seqs-empty? [seqs]
  (reduce (fn [acc a-seq] (and acc (empty? a-seq))) true seqs))

(defn firsts [seqs]
  (reduce (fn [acc a-seq] (conj acc (first a-seq))) [] seqs))

(defn rests [seqs]
  (reduce (fn [acc a-seq] (conj acc (rest a-seq))) [] seqs))

(defn my-map [f & seqs]
  (cond
   (seqs-empty? seqs) '()
   :else (cons (apply f (firsts seqs))
               (apply my-map f (rests seqs)))))
