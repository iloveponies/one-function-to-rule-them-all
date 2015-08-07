(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (if (empty? (rest a-seq))
      (first a-seq)
      (reduce (fn [str-1 str-2]
                (str str-1 " " str-2))
              (first a-seq)
              (rest a-seq)))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (if (empty? (rest a-seq))
      a-seq
      (reduce (fn [seq-1 elem]
              (conj seq-1 x elem))
            [(first a-seq)]
            (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [n seq-1]
              n
              (inc n))
          0
          a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [ret elem]
            (concat [elem] ret))
          []
          a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [ret elem]
            [(min (first ret) elem) (max (second ret) elem)])
          [100 0]
          a-seq))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn insert [sorted-seq n]
  (loop [a-seq sorted-seq
         ret []
         missing true]
    (if (and (empty? a-seq) missing)
      (conj ret n)
      (if (empty? a-seq)
        ret
        (if (and missing (< n (first a-seq)))
          (recur (rest a-seq) (conj ret n (first a-seq)) false)
          (recur (rest a-seq) (conj ret (first a-seq)) missing))))))

(defn insertion-sort [a-seq]
  (reduce (fn [ret elem]
            (insert ret elem))
          []
          a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (reduce (fn [ret elem]
            (toggle ret elem)) #{} a-seq))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& x]
  (reduce (fn [n elem]
            (inc n))
          0
          x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred] pred)
  ([pred-1 pred-2] (fn [x] (and (pred-1 x) (pred-2 x))))
  ([pred-1 pred-2 & more]
   (fn [x] (reduce (fn [ret p]
                     (and ret (p x)))
                   (and (pred-1 x) (pred-2 x))
                   more))))

(defn get-firsts [a-seq]
  (loop [ret [(first (first a-seq))]
         seqs (rest a-seq)]
    (if (empty? seqs)
      ret
      (recur (concat ret [(first (first seqs))]) (rest seqs)))))

(defn drop-firsts [a-seq]
  (loop [ret [(rest (first a-seq))]
         seqs (rest a-seq)]
    (if (empty? seqs)
      ret
      (recur (concat ret [(rest (first seqs))]) (rest seqs)))))

(defn map-multiple [f seqs]
  (loop [ret []
         cur-seqs seqs]
    (if (empty? (first cur-seqs))
      ret
      (recur (concat ret [(apply f (get-firsts cur-seqs))]) (drop-firsts cur-seqs)))))

(defn normal-map [f a-seq]
  (for [elem a-seq]
    (f elem)))

(defn my-map
  ([f a-seq]
    (normal-map f a-seq))
  ([f a-seq & rest-seqs]
   (map-multiple f (concat rest-seqs [a-seq]))))
