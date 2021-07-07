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
    (let [f (fn [first second] (conj first x second))]
      (reduce f [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [count e] (inc count))]
    (reduce counter 0 a-seq)))


(defn my-reverse [a-seq]
  (let [construct (fn [seq el] (cons el seq))]
    (reduce construct () a-seq)))

(defn min-max-element [a-seq]
  (let [comp (fn [results el]
               (cond
                (< el (first results)) [el (last results)]
                (> el (last results)) [(first results) el]
                :else results))]
    (reduce comp [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (concat (take-while (fn [x] (<= x n)) sorted-seq)
          (list n)
          (drop-while (fn [x] (>= n x)) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (let [freqs (frequencies a-seq)
        f (fn [results el] (if (odd? (get freqs el))
                             (conj results el)
                             results))]
    (reduce f #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([& more] (+ 0 (count more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & more] (reduce * x more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (fn [x]
                    (reduce (fn [prev-pred-result next-pred] (and prev-pred-result (next-pred x))) (and (p1 x) (p2 x)) more))))

(defn my-map
  ([f a-seq] (reduce (fn [seq el] (concat seq [(f el)])) () a-seq))
  ([f a-seq & more-seqs] (loop [seqs (concat (list a-seq) more-seqs)
                                acc (empty more-seqs)]
                           (if (every? (complement nil?)
                                       (map (partial not-empty) seqs))
                             (recur (my-map rest seqs)
                                    (concat acc [(apply f (my-map first seqs))]))
                             acc))))
