(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str
            (reduce
              (fn [a b] (concat a " " b))
              a-seq))))

(defn my-interpose [x a-seq]
  (rest
    (reduce
      (fn [a b] (conj a x b)) [] a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                  (if (nil? e)
                    count
                    (inc count)))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [switch (fn [a b] (cons b a))]
    (reduce switch '() a-seq)))

(defn min-max-element [a-seq]
  (let [min-max-fn (fn [min-max-vec x]
                     (cond (nil? min-max-vec) [x x]
                           (> (first min-max-vec) x) (assoc min-max-vec 0 x)
                           (< (second min-max-vec) x) (assoc min-max-vec 1 x)
                           :else
                           min-max-vec))]
    (reduce min-max-fn nil a-seq)))

(defn insert [sorted-seq n]
  (loop [acc '() s sorted-seq]
    (cond (empty? s) (concat acc (list n))
          (<= n (first s)) (concat acc (list n) s)
          :else
          (recur (concat acc (list (first s))) (rest s))
          )))

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

(defn count-params
  ([] 0)
  ([_ & more]
   (reduce (fn [count _] (inc count)) 1 more)))

(defn my-*
  ([] 1)
  ([x & more]
   (reduce (fn [prod _] (* prod _)) x more)))

(defn pred-and
  ([] (fn [_] true))
  ([x y] (fn [a] (and (x a) (y a))))
  ([x y & more]
   (reduce pred-and (pred-and x y) more)))

(defn all-empty? [seqs]
  (reduce #(and %1 (empty? %2)) true seqs))

(defn first-of [seqs]
  (reduce #(conj %1 (first %2)) [] seqs))

(defn rest-of [seqs]
  (reduce #(conj %1 (rest %2)) [] seqs))

(defn my-map [f & seqs]
  (cond (all-empty? seqs) '()
        :else
        (cons (apply f (first-of seqs))
              (apply my-map (cons f (rest-of seqs))))))
