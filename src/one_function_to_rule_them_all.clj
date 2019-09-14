(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (> 1 (count a-seq))
    '()
    (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [n elem] (inc n))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce #(conj %1 %2) '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (let [min-elem (first a-seq)
          max-elem (first a-seq)
          compare-and-replace (fn [existing-min-max to-compare]
                                (cond
                                  (> (first existing-min-max) to-compare)
                                    [to-compare (second existing-min-max)]
                                  (< (second existing-min-max) to-compare)
                                    [(first existing-min-max) to-compare]
                                  :else
                                    existing-min-max))]
      (reduce compare-and-replace [min-elem max-elem] a-seq))))

(defn insert [sorted-seq n]
  (concat (take-while #(< % n) sorted-seq)
          (list n)
          (drop-while #(< % n) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([y x] (- y x)))

(defn count-params [& x]
  (count x))

(defn my-* [& x]
  (reduce * x))

(defn pred-and
  ([] (fn [y] true))
  ([pred] pred)
  ([pred1 pred2] (fn [y] (and (pred1 y) (pred2 y))))
  ([pred1 pred2 & others] (fn [y]
                            (reduce #(and %1 (%2 y))
                                    ((pred-and pred1 pred2) y)
                                    others))))

(defn firsts-and-rests [seqs]
  (loop [firsts '()
         rests '()
         remaining seqs]
    (if (empty? remaining)
      (list firsts rests)
      (let [this-seq (first remaining)]
        (recur (concat firsts (list (first this-seq)))
               (concat rests (list (rest this-seq)))
               (rest remaining))))))

(defn my-map [f & seqs]
  (loop [[firsts rests] (firsts-and-rests seqs)
         results '()]
    (if (nil? (first firsts))
      results
      (recur (firsts-and-rests rests)
             (concat results (list (apply f firsts)))))))
