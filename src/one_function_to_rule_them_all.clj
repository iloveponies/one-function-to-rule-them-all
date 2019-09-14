(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq ))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (reverse (rest (reduce #(conj (conj %1 %2) x) '() a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [counter term] (inc counter)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[x y] z]
            [(min x z) (max y z)]) [(first a-seq) (first a-seq)] (rest a-seq)))

(defn insert-helper [acc sorted-seq n]
  (if (empty? sorted-seq)
    (concat acc (list n))
    (let [elem (first sorted-seq)]
      (if (< n elem)
        (concat acc (cons n sorted-seq))
        (insert-helper (concat acc (list elem)) (rest sorted-seq) n)))))

(defn insert [sorted-seq n]
  (insert-helper '() sorted-seq n))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce (fn [set elem]
            (if (contains? set elem)
              (disj set elem)
              (conj set elem))) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [ & more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
     (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [z] true))
  ([x] (fn [z] (x z)))
  ([x y] (fn [z] (and (x z) (y z))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn arg-helper 
  "Return a vector of args to be passed to the function f of my-map as
   well as a vector consisting of the rest of the lists in the seq-of-seqs"
  [seq-of-seqs]
  (loop [my-coll seq-of-seqs
         args []
         seq-of-rests []]
    (cond
     ;; exhausted sequences without running out of args for f
     (empty? my-coll) [args seq-of-rests]
     ;; one of the sequences is empty--no args for f
     (empty? (first my-coll)) nil
     :else (let [[head & tail] (first my-coll)]
             (recur (rest my-coll)
                    (conj args head)
                    (conj seq-of-rests tail))))))

(defn my-map [f & a-seq-of-seqs]
  (loop [my-coll a-seq-of-seqs
         result []]
    (let [params (arg-helper my-coll)]
      (if (nil? params)
        (seq result)
        (let [[head & tails] params]
          (recur (first tails) (conj result (apply f head))))))))
