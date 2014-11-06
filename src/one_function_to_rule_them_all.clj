(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
  (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (reduce #(if (empty? %1)
       (conj %1 %2)
       (conj %1 x %2)) []  a-seq))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                    (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (vec [(reduce min a-seq) (reduce max a-seq)]))

(defn insert [sorted-seq n]
  (loop [m [n]
         newseq []
         coll sorted-seq]
    (if (empty? coll)
      (if (not (empty? m))
       (conj newseq (first m))
       newseq)
      (if (and (not (empty? m)) (> (first coll) (first m)))
        (recur (rest m) (conj newseq (first m) (first coll)) (rest coll))
        (recur  m (conj newseq (first coll)) (rest coll))))))


(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [acc #{}
       f (fn [x y] (if (contains? x y) (disj x y) (conj x y)))]
       (reduce f acc a-seq)))

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
