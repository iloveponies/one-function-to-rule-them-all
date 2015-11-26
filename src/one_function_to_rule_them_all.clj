(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (let [
        filtered-seq (filter (fn [x] (not (empty? x))) a-seq)
        ]
    (if (empty? filtered-seq) ""
                              (reduce (fn [x y] (str x " " y)) (first a-seq) (rest a-seq))
                              )
  ))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) a-seq
                    (reduce (fn [my-seq el] (conj (conj my-seq x) el)) [(first a-seq)] (rest a-seq)))
  )

(defn my-count [a-seq]
  (reduce (fn [cnt x] (inc cnt)) 0 a-seq)
  )

(defn my-reverse [a-seq]
  (reduce #(conj %1 %2) '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq) [nil nil]
                     (reduce (fn [[mi ma] el] [(min mi el) (max ma el)]) [(first a-seq) (first a-seq)] (rest a-seq))
   ))

(defn insert [sorted-seq n]
  (concat (filter (fn [x] (< x n)) sorted-seq) (concat [n] (filter (fn [x] (>= x n)) sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [s x] (if (contains? s x) (disj s x) (conj s x))) (set nil) a-seq)
  )

(defn minus
  ([x] (minus 0 x))
  ([x y] (- x y))
  )

(defn count-params [& x]
  (count x))

(defn my-* [& x]
  (reduce * 1 x))

(defn pred-and [& x]
  (fn [val]
    (let [combined-pred (reduce (fn [a b] (fn [c] (and (a c) (b c))))
                                (fn [a] true)
                                x)]
      (combined-pred val)
    )))

(defn get-first-els [ & seqs]
  (loop [first-els []
         rem-seqs  []
         source-seqs seqs]
    (if (empty? source-seqs) [first-els rem-seqs]
                          (recur (conj first-els (first (first source-seqs)))
                                 (conj rem-seqs (rest (first source-seqs)))
                                 (rest source-seqs)))))

(defn my-map [f & a-seq]
  (if (empty? a-seq) nil
  (loop [
         mapped-seq []
         rem-seq a-seq
         ]
    (if (empty? (first rem-seq)) mapped-seq
                               (let [
                                     [first-el-seq rest-seq] (apply get-first-els rem-seq)
                                     ]
                                 (recur (conj mapped-seq (apply f first-el-seq)) rest-seq)
    )))))