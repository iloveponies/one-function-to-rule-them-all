(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (rest (seq (reduce #(conj %1 x %2) [] a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [x _] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (let [mm (fn [[mi ma] x]
             [(min x mi) (max x ma)])]
    (if (empty? a-seq)
      nil
      (reduce mm [(first a-seq) (first a-seq)] (rest a-seq)))))

(defn insert [sorted-seq n]
  (concat
   (take-while #(< %1 n) sorted-seq)
   [n]
   (drop-while #(< %1 n) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& xs]
  (count xs))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & pmore]
   (reduce #(fn [x] (and (%1 x) (%2 x))) (pred-and p1 p2) pmore))
  )

(defn firsts [seqs]
  (reduce #(conj %1 (first %2)) [] seqs))

(defn rests [seqs]
  (reduce #(conj %1 (rest %2)) [] seqs))

(defn all-empty? [seqs]
  (reduce #(and %1 (empty? %2)) true seqs))

(defn my-map [f & seqs]
  (if (all-empty? seqs)
    '()
    (cons (apply f (firsts seqs)) (apply my-map (cons f (rests seqs))))))

