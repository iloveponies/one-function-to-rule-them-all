(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (let [f (fn [acc e]
            (if (not (empty? acc))
              (concat acc (list x e))
              (list e)))]
    (reduce f () a-seq)))

(defn my-count [a-seq]
  (let [f (fn [acc _]
            (inc acc))]
    (reduce f 0 a-seq)))

(defn my-reverse [a-seq]
  (let [f (fn [acc elem]
            (cons elem acc))]
    (reduce f () a-seq)))

(defn min-max-element [a-seq]
  (let [f (fn [[mn mx] elem]
            (let [new-mn (if mn (min mn elem) elem)
                  new-mx (if mx (max mx elem) elem)]
              [new-mn new-mx]))]
    (reduce f [nil nil] a-seq)))

(defn insert [sorted-seq n]
  (let [lhs (take-while #(< % n) sorted-seq)
        rhs (drop (count lhs) sorted-seq)]
    (concat lhs (list n) rhs)))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (let [f (fn [acc e]
            (if (contains? acc e)
              (disj acc e)
              (conj acc e)))]
    (reduce f #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-*
  ([] 1)
  ([& args] (reduce * args)))

(defn pred-and [& preds]
  (fn [e]
    (every? identity (map #(%1 e) preds))))

(defn my-map [f & seqs]
  (loop [seqs seqs
         res ()]
    (if (every? empty? seqs)
      (reverse res)
      (recur (map rest seqs)
             (cons (apply f (map first seqs)) res)))))
