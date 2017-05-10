(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  ;; or just `(clojure.string/join " " a-seq)`
  (if (empty? a-seq)
    ""
    (let [cat (fn [sum new]
                (str sum " " new))]
      (reduce cat a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (let [interposer (fn [sum elem]
                       (conj sum x elem))]
      ;; Could someone please merge some PRs? Instructions say `'()` but tests
      ;; expect `[]`..........................................................
      (reduce interposer [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [c elem]
                  (inc c))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverse-adder (fn [sum elem]
                        (conj sum elem))]
    (reduce reverse-adder '() a-seq)))

(defn min-max-element [a-seq]
  (let [min-max-counter (fn [extremes
                             elem]
                          [(min (nth extremes 0) elem) (max (nth extremes 1) elem)])]
    (reduce min-max-counter [(Integer/MAX_VALUE) (Integer/MIN_VALUE)] a-seq)))

(defn insert [sorted-seq n]
  [:-])

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  [:-])

(defn minus [x]
  :-)

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
