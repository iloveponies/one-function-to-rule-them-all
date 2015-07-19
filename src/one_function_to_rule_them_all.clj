(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (let [interpose-x (fn [set e]
                        (conj (conj set x) e))
          start-seq [(first a-seq)]]
      (reduce interpose-x start-seq (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [n e] (inc n))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [r-seq e]
                   (cons e r-seq))]
    (reduce reverser [] a-seq)))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [first-elem (first a-seq)
          min-max-helper
          (fn [[min max] e]
            [(if (< e min) e min) (if (> e max) e max)])]
      (reduce min-max-helper [first-elem first-elem] (rest a-seq)))))

(defn insert [sorted-seq n]
  (loop [pre-seq []
         post-seq sorted-seq]
    (cond
     (empty? post-seq)       (conj pre-seq n)
     (< n (first post-seq))  (concat (conj pre-seq n) post-seq)
     :else                   (recur (conj pre-seq (first post-seq))
                                    (rest post-seq)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [parity-helper (fn [parity-set e]
                        (if (contains? parity-set e)
                          (disj parity-set e)
                          (conj parity-set e)))]
    (reduce parity-helper #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-*
  ([] 1)
  ([x & more] (reduce * x more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (let [ps (list* p1 p2 more)]
                    (fn [x] (reduce #(and %1 (%2 x)) true ps)))))

(defn my-map
  ([f a-seq] (reduce #(conj %1 (f %2)) [] a-seq))
  ([f a-seq & more] (let [seqs (cons a-seq more)
                          xs (loop [coll []
                                    x seqs]
                               (if (every? seq x)
                                 (recur (conj coll (my-map first x)) (my-map rest x))
                                 coll))]
                      (reduce #(conj %1 (apply f %2)) [] xs))))






