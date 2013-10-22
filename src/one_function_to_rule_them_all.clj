(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) "" (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
      ()
      (let [[a & a-seq] a-seq]
        (concat (list a) (reduce (fn [acc a] (conj acc x a)) [] a-seq)))))

(defn my-count [a-seq]
  (reduce + 0 (map (constantly 1) a-seq)))

(defn my-reverse [a-seq]
  (reduce (fn [acc b] (cons b acc)) () a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[amin amax] a]
            [(min a (or amin a))
             (max a (or amax a))])
          [nil nil]
          a-seq))

(defn insert [sorted-seq n]
  ((fn [acc tail]
     (if (or (empty? tail) (< n (first tail)))
         (concat acc [n] tail)
         (recur (conj acc (first tail))
                (rest tail))))
   [] sorted-seq))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (reduce (fn [odds a]
            (if (contains? odds a)
                (disj odds a)
                (conj odds a)))
          (set [])
          a-seq))

(defn minus
  ([x] (- x))
  ([a b] (- a b)))

(defn count-params [& xs]
  (count xs))

(defn my-* [& xs]
  (reduce * 1 xs))

(defn pred-and [& ps]
  (fn [x] (reduce (fn [ok? p] (and ok? (p x))) true ps)))

(defn my-map [f & seqs]
  (let [collect (fn [f xs] (reduce (fn [coll x] (conj coll (f x))) [] xs))]
    ((fn [results tails]
       (if (some empty? tails)
           (reverse results)
           (let [heads (collect first tails)
                 tails (collect rest tails)]
             (recur (conj results (apply f heads))
                    tails))))
     () seqs)))
