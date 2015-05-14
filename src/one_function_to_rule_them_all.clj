(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (let [join (fn [s elem] (str s " " elem))]
    (if (empty? a-seq)
      ""
      (reduce join a-seq))))

(defn my-interpose [x a-seq]
  (let [join (fn [s elem] (conj s x elem))]
     (if (empty? a-seq)
       '()
       (reverse (reduce join (conj '() (first a-seq)) (rest a-seq))))))

(defn my-count [a-seq]
  (let [counter (fn [count elem]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [enqueue (fn [s elem]
                  (cons elem s))]
    (reduce enqueue '() a-seq)))

(defn min-max-element [a-seq]
  (let [min-max (fn [v elem]
                  (cond
                    (or (nil? (second v)) (> elem (second v))) (assoc v 1 elem)
                    (or (< elem (first v))) (assoc v 0 elem)
                    :else v))]
    (reduce min-max [(first a-seq) (second a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (let [[start end] (split-with #(< % n) sorted-seq)]
    (concat start [n] end)))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [f (fn [s elem]
            (if (contains? s elem)
              (disj s elem)
              (conj s elem)))]
    (reduce f #{} a-seq)))

(defn minus
  ([x] (- x (* 2 x)))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce * (* x y) more)))

(defn pred-and
  ([& more]
    (fn [x]
      (or (every? (fn [p] (p x)) more)))))

(defn my-map [f a-seq]
  [:-])
