(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat "" a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [word remn]
              (str word " " remn))
                a-seq)))

(defn my-interpose [x a-seq]
  (cond
   (empty? a-seq)
     '()
   (== 1 (count a-seq))
     (seq a-seq)
   :else
     (reduce (fn [elem1 tail]
               (conj elem1 x tail))
                 (vector (first a-seq))
                 (rest a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [n seqx]
                  (inc n))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (vector (reduce min a-seq) (reduce max a-seq)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (seq [n])
    (if (<= n (first sorted-seq))
      (cons n sorted-seq)
      (cons (first sorted-seq)
            (insert (rest sorted-seq) n)))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [elems eka]
                 (if (contains? elems eka)
                   (disj elems eka)
                   (conj elems eka)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

((defn count-params [& more]
  (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([x] x)
  ([x y] (fn [elem] (and (x elem) (y elem))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn my-map [f a-seq]
  [:-])

(defn my-map3 [f & seqs]
  (let [seqlooper (fn [x] (loop [firstseq (first x)
                                 restseqs (rest x)
                                 return   '()]
                            (if (empty? restseqs)
                              (conj return (first firstseq))
                              (recur (first restseqs)
                                     (rest restseqs)
                                     (conj return (first firstseq))))))]
