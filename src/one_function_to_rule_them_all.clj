(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (let [join (fn [acc value]
               (conj acc x value))]
    (if (empty? a-seq)
      []
      (reduce join [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev (fn [reversed e]
              (cons e reversed))]
    (reduce rev [] a-seq)))

(defn min-max-element [a-seq]
  (let [min-max (fn [mM e]
                  (let [[mm MM] mM]
                      (cond
                        (empty? mM) [e e]
                        (< e mm)    [e MM]
                        (> e MM)    [mm e]
                        :else       mM)))]
  (reduce min-max [] a-seq)))

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq)      [n]
    (< n (first sorted-seq)) (cons n sorted-seq)
    :else                    (cons (first sorted-seq)
                                   (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x]   (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([& more]
    (let [increase (fn [acc e] (inc acc))]
      (reduce increase 0 more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce my-* (my-* x y) more)))

(defn pred-and
  ([]
    (fn [e] true))
  ([x]
    (fn [e] (x e)))
  ([x y]
    (fn [e]
      (and (x e) (y e))))
  ([x y & more]
    (reduce pred-and (pred-and x y) more)))

(defn heads-and-tails [a-seq]
  (loop [heads []
         tails []
         rests a-seq]
    (cond
      (empty? rests)
        [heads tails]
      (empty? (first rests))
        [[] a-seq]
      :else
        (recur (conj heads (first (first rests)))
               (conj tails (rest (first rests)))
               (rest rests)))))

(defn my-map [f & seqs]
  (loop [result []
         rests  seqs]
    (let [[heads tails] (heads-and-tails rests)]
      (if (empty? heads)
        result
        (recur (conj result (apply f heads))
               tails)))))
