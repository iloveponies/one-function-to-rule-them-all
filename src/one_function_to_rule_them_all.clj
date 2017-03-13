(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (let [catenator (fn [s1 s2]
                 (str s1 " " s2))]
    (if (empty? a-seq)
      ""
      (reduce catenator a-seq))))

(defn my-interpose [x a-seq]
  (let [interposer (fn [front tail]
                 (conj (conj front x) tail))]
    (if (empty? a-seq)
      []
      (reduce interposer [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [count e] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (let [conjucator (fn [front tail]
                 (conj front tail))]
    (if (empty? a-seq)
      '()
      (reduce conjucator (list (first a-seq)) (rest a-seq)))))

(defn min-max-element [a-seq]
  (let [min-max-tester (fn [min-max elem]
                         [(min (first min-max) elem)
                          (max (second min-max) elem)])]
    (reduce min-max-tester [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [front []
         tail sorted-seq]
    (cond
      (empty? tail)
        (conj front n)
      (>= (first tail) n)
        (apply conj (conj front n) tail)
      :else
        (recur (conj front (first tail)) (rest tail))
      )))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [freqs (frequencies a-seq)
        element-adder (fn [odd-set elem]
                        (if (odd? (get freqs elem))
                          (conj odd-set elem)
                          odd-set))]
    (reduce element-adder #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (reduce (fn [count e] (inc count)) 0 params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (fn [x] (let [trueth-checker (fn [trueth-holder pred]
                                                 (and trueth-holder (pred x)))]
                            (reduce trueth-checker (and (p1 x) (p2 x)) more)))))

(defn my-map
  ([f a-seq] (let [func-applier (fn [accumulator elem]
                                  (conj accumulator (f elem)))]
               (reduce func-applier [] a-seq)))
  ([f a-seq & more]
  (let [sequence-combiner (fn [accumulator a-seq]
                            (loop [new-accumulator []
                                   accumulator-tail accumulator
                                   seq-tail a-seq]
                              (if (empty? seq-tail)
                                new-accumulator
                                (recur (conj new-accumulator
                                             (conj (first accumulator-tail) (first seq-tail)))
                                       (rest accumulator-tail)
                                       (rest seq-tail)))))
        elem-lists (reduce sequence-combiner (my-map list a-seq) more)]
    (let [func-applier (fn [accumulator elem]
                         (conj accumulator (apply f elem)))]
      (reduce func-applier [] elem-lists)))))
