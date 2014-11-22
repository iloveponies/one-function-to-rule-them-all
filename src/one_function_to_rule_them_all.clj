(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [s1 s2] (str s1 " " s2)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce 
    (fn [accum e]
      (if (empty? accum)
        [e]
        (conj accum x e)))
    [] a-seq))

(defn my-count [a-seq]
  (let [count-fn (fn [count e]
                   (if (nil? e)
                     count
                     (inc count)))]
    (reduce count-fn 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev-fn (fn [s e]
                 (if (nil? e)
                   s
                   (conj s e)))]
    (reduce rev-fn `() a-seq)))

(defn min-max-element [a-seq]
  (let [min-max-fn (fn [min-max-vec e]
                     (if (empty? min-max-vec)
                       [e e]
                       (cond
                         (< e (first min-max-vec)) (assoc min-max-vec 0 e)
                         (> e (last min-max-vec)) (assoc min-max-vec 1 e)
                         :else min-max-vec)))]
    (reduce min-max-fn [] a-seq)))

(defn insert [sorted-seq n]
  (let [less-seq (filter (fn [e] (< e n)) sorted-seq)
        more-seq (filter (fn [e] (> e n)) sorted-seq)]
    (concat less-seq [n] more-seq)))

(defn insertion-sort [a-seq]
  (reduce (fn [sorted-seq n]
            (insert sorted-seq n)) [] a-seq))

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