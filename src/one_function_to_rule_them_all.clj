(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [s1 s2] (str s1 " " s2)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (let [poser (fn [s y]
                    (if (empty? s)
                      (concat s (cons y ()))
                      (concat s (cons x (cons y ())))))]
    (reduce poser () a-seq))))

(defn my-count [a-seq]
  (reduce (fn [c s] (inc c)) 0 a-seq))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    ()
    (let [flipper (fn [s x] (concat (cons x ()) s))]
      (reduce flipper () a-seq))))

(defn min-max-element [a-seq]
  (let [helper (fn [minmax x]
                 (conj [] (min (first minmax) x)
                          (max (second minmax) x)))
        a (first a-seq)]
    (reduce helper (conj [] a a) a-seq)))

(defn insert [sorted-seq n]
  (loop [head-seq []
         tail-seq sorted-seq]
    (if (or (empty? tail-seq)
            (<= n (first tail-seq)))
      (concat head-seq (cons n tail-seq))
      (recur (conj head-seq (first tail-seq))
             (rest tail-seq)))))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (my-count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [& args] true))
  ([pred1] pred1)
  ([pred1 pred2]
    (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more]
    (reduce pred-and (pred-and pred1 pred2) more)))

(defn my-map [f & seqs]
  (let [firsts-seq (fn [seqs]
                     (reduce #(conj %1 (first %2)) [] seqs))
        rests-seqs (fn [seqs]
                     (reduce #(conj %1 (rest %2)) [] seqs))]
    (if (some empty? seqs)
      ()
      (cons (apply f (firsts-seq seqs))
            (apply my-map f (rests-seqs seqs))))))
