(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [helper (fn [str1 str2] (str str1 " " str2))]
      (reduce helper a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
  (let [helper (fn [vec1 elem2] (conj vec1 x elem2))]
    (reduce helper (vector (first a-seq)) (rest a-seq)))))

(defn my-count [a-seq]
  (if (empty? a-seq)
    0
    (let [helper (fn [count e] (inc count))]
      (reduce helper 0 a-seq))))

(defn my-reverse [a-seq]
 (let [helper (fn [r-seq elem] (cons elem r-seq))]
  (reduce helper [] a-seq)))


(defn min-max-element [a-seq]
  (let [helper (fn [[min max] elem] (cond
                                     (> elem max) [min elem]
                                     (< elem min) [elem max]
                                     :else [min max]))]
    (reduce helper (vector (first a-seq) (first a-seq)) (rest a-seq))))

(defn insert [sorted-seq n]
  (cond (empty? sorted-seq) (list n)
        (< n (first sorted-seq)) (cons n sorted-seq)
        :else (cons (first sorted-seq)
                    (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
 (let [freqs (frequencies a-seq)
       helper (fn [res-seq elem-freq] (if (odd? (second elem-freq))
                                        (conj res-seq (first elem-freq))
                                        res-seq))]
   (set (reduce helper [] freqs))))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and [& more]
  (fn [x] (loop [preds more]
            (cond (empty? preds) true
                  ((first preds) x) (recur (rest preds))
                  :else false))))



(defn helper-my-map [f a-seq]
  (if (empty? (first a-seq))
    '()
    (cons (apply f (map first a-seq))
          (helper-my-map f (map rest a-seq)))))

(defn my-map
  ([f a-seq] (map f a-seq))
  ([f a-seq & more] (helper-my-map f (concat [a-seq] more))))

