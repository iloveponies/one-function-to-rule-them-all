(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
      ""
    (let [cat (fn [seq1 seq2] (str seq1 " " seq2))]
      (reduce cat a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
      a-seq
    (let [cat (fn [seq1 seq2] (conj seq1 x seq2))]
      (reduce cat [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                  (if (not (nil? e))
                    (inc count)
                    count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev (fn [out e]
                  (if (nil? e)
                    out
                    (cons e out)
                    ))]
    (reduce rev [] a-seq)))

(defn min-max-element [a-seq]
  (let [firste (first a-seq)
         min-max (fn [out-seq e]
                  (cond
                    (> (get out-seq 0) e)
                      (assoc out-seq 0 e)
                    (< (get out-seq 1) e)
                      (assoc out-seq 1 e)
                    :else
                      out-seq
                    ))]
    (reduce min-max [firste firste] (rest a-seq))))

(defn my-take [n coll]
  (cond (zero? n)
      []
    (> n (count coll))
      (my-take (count coll) coll)
    :else
      (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond (or (empty? coll) (> n (count coll)))
      ()
    (> n 0)
      (my-drop (dec n) (rest coll))
    :else
      (cons (first coll) (my-drop 0 (rest coll)))))

(defn insert [sorted-seq n]
  (loop [acc 0]
    (cond
      (= acc (count sorted-seq))
        (concat sorted-seq [n])
      (< n (get sorted-seq acc))
        (concat (my-take acc sorted-seq) [n] (my-drop acc sorted-seq))
      :else
        (recur (inc acc)))))

(defn insertion-sort [a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [sorted e] (insert (vec sorted) e)) [] a-seq)))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
    (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
      (reduce toggle (set nil) a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x y] 2)
  ([x y & more]
    (reduce (fn [acc e] (inc acc)) (count-params x y) more)))

(defn my-*
  ([] 1)
  ([x] 1)
  ([x y] (* x y))
  ([x y & more]
    (reduce * (my-* x y) more)))

(defn pred-and
  ([] (fn [a] true))
  ([x] (fn [a] (x a)))
  ([x y] (fn [a] (and (x a) (y a))))
  ([x y & more]
    (reduce (fn [p q] (fn [a] (and (p a) (q a)))) (pred-and x y) more)))

(defn my-map [f a-seq]
  [:-])
