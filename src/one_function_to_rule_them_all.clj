(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (reduce (fn [acc, string]
            (str acc " " string))
          (if (empty? a-seq)
            [""]
            a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [acc, el]
              (conj (conj acc x) el))
            [(first a-seq)]
            (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [count _]
            (inc count))
          0
          a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc el]
            (cons el acc))
          []
          a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[cur-min cur-max] el]
            [(min cur-min el) (max cur-max el)])
          [(first a-seq) (first a-seq)]
          (rest a-seq)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
    (let [f (fn [x] (< x n))
          under (take-while f sorted-seq)
          over (drop-while f sorted-seq)]
      (concat under [n] over))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [acc el]
                 (if (contains? acc el)
                   (disj acc el)
                   (conj acc el)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& xs]
  (count xs))

(defn my-* [& xs]
  (reduce * 1 xs))

(defn pred-and [& preds]
  (reduce (fn [result pred]
            (fn [x] (and (result x)
                         (pred x))))
          (fn [x] true)
          preds))

(defn my-map [f & seqs]
  (let [combine (fn [& seqs]
                  (reduce #(loop [result []
                                  cur-acc %1
                                  cur-seq %2]
                             (if (empty? cur-seq)
                               result
                               (recur (conj result (conj (first cur-acc)
                                                         (first cur-seq)))
                                      (rest cur-acc)
                                      (rest cur-seq))))
                          (repeat (count seqs) [])
                          seqs))]
  (reduce #(conj %1 (apply f %2))
          []
          (apply combine seqs))))
