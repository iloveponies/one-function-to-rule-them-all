(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (butlast
     (reduce
      (fn [res elem] (conj (conj res elem) x))
      []
      a-seq))))

(defn my-count [a-seq]
  (reduce (fn [cnt elem] (inc cnt)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [rev-seq elem] (conj rev-seq elem)) '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (reduce
     (fn [[min-elem max-elem] elem]
       (cond
        (< elem min-elem) [elem max-elem]
        (> elem max-elem) [min-elem elem]
        :else [min-elem max-elem]))
     [(first a-seq) (first a-seq)]
     a-seq)))

(defn insert [sorted-seq n]
  (let [helper (fn [start n end]
                 (cond
                  (empty? end) (concat start (list n))
                  (> (first end) n) (concat start (list n) end)
                  :else (recur (concat start (take 1 end))
                               n
                               (rest end))))]
    (helper '() n sorted-seq)))

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
  ([x y] (- x y)))

(defn count-params [& params]
  (reduce (fn [cnt elem] (inc cnt)) 0 params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
    (reduce pred-and (pred-and p1 p2) more)))

(defn my-map [f sq & seqs]
  (let [zip (fn [[heads tails]]
              (if (some empty? tails)
                (reverse heads)
                (recur (reduce (fn [[h t] sq]
                                 [(cons (concat (first h) (list (first sq)))
                                        (rest h))
                                  (concat t (list (rest sq)))])
                               [(conj heads '()) '()]
                               tails))))]
    (reduce (fn [acc elem] (concat acc (list (apply f elem))))
            '()
            (zip ['() (cons sq seqs)]))))
