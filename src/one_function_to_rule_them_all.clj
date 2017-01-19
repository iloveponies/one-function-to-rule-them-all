(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce #(concat %1 [x %2]) (list (first a-seq)) (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc el] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (let [f (first a-seq)
        r (rest a-seq)
        helper (fn [ vec n]
                 (let [ [small big] vec]
                   [(min n small) (max n big)]))]

    (reduce helper [f f] r)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (list n)

    (loop [my-vec []
           sorted-seq sorted-seq
           rest-s (rest sorted-seq)
           f (first sorted-seq)]
      (cond
        (empty? sorted-seq) (concat my-vec [n])
        (<= n f) (concat my-vec [n f] rest-s)
        :else (recur (conj my-vec f) rest-s (rest rest-s) (first rest-s))))))


(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [acc el]
                 (if (contains? acc el)
                  (disj acc el)
                  (conj acc el)))]

    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& args]
  (reduce * 1 args))

(defn pred-and [& args]
  (fn [el]
    (loop [funk (first args)
           args (rest args)]
      (cond
        (nil? funk) true
        (not (funk el)) false
        :else (recur (first args) (rest args))))))

(defn seq-rest [s]
  (let [helper (fn [acc ss]
                 (conj acc (rest ss)))]

    (reduce helper [] s)))

(seq-rest '([1 2 3] [2 3 4]))

(defn seq-firsts [s]
  (let [helper (fn [acc ss]
                 (conj acc (first ss)))]

    (reduce helper [] s)))

(seq-firsts '([1 2 3] [2 3 4]))

(defn my-map [f & seqs]
  (loop [acc []
         seqs seqs]
    (if (> (count (filter empty? seqs)) 0)
      acc
      (recur (conj acc (apply f (seq-firsts seqs)))
             (seq-rest seqs)))))
