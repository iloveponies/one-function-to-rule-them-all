(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b]
              (clojure.string/join " " [a b]))
            (first a-seq)
            (rest a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [a b]
             (conj (into [] a) x b))
            (list (first a-seq))
            (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [c e]
            (inc c))
          0
          a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [s x]
            (cons x s))
          '()
          a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[mn mx] e]
            (cond
              (> e mx) [mn e]
              (< e mn) [e mx]
              :else [mn mx]))
          [(first a-seq) (first a-seq)]
          a-seq))

(defn insert [sorted-seq n]
  (let [helper (fn [acc ss nn i?]
                 (cond
                   (and (empty? ss) i?) (reverse acc)
                   (empty? ss) (recur (cons nn acc) ss nn true)
                   i? (recur (cons (first ss) acc) (rest ss) nn i?)
                   :else (if (< nn (first ss))
                           (recur (cons nn acc) ss nn true)
                           (recur (cons (first ss) acc) (rest ss) nn false))))]
    (helper '() sorted-seq n false)))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set e]
                 (if (contains? a-set e)
                   (disj a-set e)
                   (conj a-set e)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & z]
   (reduce * (* x y) z)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p & ps] (fn [x]
              (reduce (fn [b pp]
                        (and b (pp x)))
                      (p x)
                      ps))))

(defn my-map [f & seqs]
  (let [slice (fn [soss]
                (let [[c r e?] (if (empty? soss)
                                 ['() '() true]
                                 (reduce (fn [[cc rr ee?] se]
                                           (cond
                                             ee? [cc rr ee?]
                                             (empty? se) [cc rr true]
                                             :else [(cons (first se) cc)
                                                    (cons (rest se) rr) ee?]))
                                         ['() '() false]
                                         soss))]
                  [(reverse c) (reverse r) e?]))
        reshape-args (fn [soss]
                       (loop [sos soss
                              na '()]
                         (let [[c r e?] (slice sos)]
                           (if e?
                             (reverse na)
                             (recur r (cons c na))))))]
    (reverse (reduce (fn [acc lst]
                       (cons (apply f lst) acc))
                     '()
                     (reshape-args seqs)))))
