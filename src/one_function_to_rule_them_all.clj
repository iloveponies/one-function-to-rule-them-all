(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce #(concat %1 %2) `() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)
    ))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) `() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [update-min-max (fn [[current-min current-max] new-element]
                           [(min current-min new-element)
                            (max current-max new-element)])
          initial (first a-seq)]
      (reduce update-min-max [initial initial] a-seq))))

(defn insert [sorted-seq n]
  (let [[lower higher] (split-with #(< %1 n) sorted-seq)]
    (concat lower [n] higher)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce #(toggle %1 %2) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-* [& x]
  (reduce * 1 x))

(defn pred-and [& predicates]
  (let [combine-predicates (fn [a b]
                             (fn [element] (and (a element) (b element))))
        always-true-predicate (fn [_] true)]
    (reduce combine-predicates always-true-predicate predicates)))

(defn my-map [f & a-seq]
  (loop [mapper f
         a-seq a-seq
         current-results []]
    (if (every? empty? a-seq)
      current-results
      (let [firsts (map first a-seq)
            new-element (apply mapper firsts)
            rests (map rest a-seq)]
        (recur mapper rests (conj current-results new-element))))))