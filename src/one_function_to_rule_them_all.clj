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
  (let [min-max-counter (fn [{maximum :max
                              minimum :min} elem]
                          {:max (max maximum elem) :min (min minimum elem)})]
    ;; Transform the map to an array
    (let [{:keys [min max]}
          (reduce min-max-counter {:min (Integer/MAX_VALUE) :max (Integer/MIN_VALUE)} a-seq)]
      [min max])))

(defn insert [sorted-seq n]
  (loop [new-seq '()
         n n
         sorted-seq sorted-seq]
    (if (not n)
      new-seq
      (if (empty? sorted-seq)
        (concat new-seq [n])
        (let [f (first sorted-seq)]
          (if (<= n f)
            (concat new-seq [n f] (rest sorted-seq))
            (recur (concat new-seq [f]) n (rest sorted-seq))))))))

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
