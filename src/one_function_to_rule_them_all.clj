(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (cond
   (empty? a-seq) '()
   (= 1 (count a-seq)) a-seq
   :else (reduce (fn [y z]
                (let [the-list (if (list? y)
                                    (conj y x z)
                                    (list y x z))]
                  (reverse the-list)))
            a-seq)))

(defn my-count [a-seq]
  (let [counter (fn[cnt e](inc cnt))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn[new-list e](conj new-list e))]
    (reduce reverser '() a-seq)))

(defn nil-or-true [n pred tester]
  (or (nil? tester)(pred n tester)))

(defn min-max-element [a-seq]
  (let [miner (fn [min-max e] (if (nil-or-true e <= (first min-max)) [e (last min-max)] min-max))
         maxer (fn [min-max e] (if (nil-or-true e >= (last min-max)) [(first min-max) e] min-max))
         min-maxer (fn [min-max e](maxer (miner min-max e) e))]
   (reduce min-maxer [] a-seq)))

(defn insert [sorted-seq n]
  [:-])

(defn insertion-sort [a-seq]
  [:-])

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
