(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (loop [target-seq []
         source-seq a-seq]
    (if (empty? source-seq)
      target-seq
      (recur (concat target-seq (first source-seq)) (rest source-seq)))))

(defn str-cat [a-seq]
  :-)

(defn my-interpose [x a-seq]
  [:-])

(defn my-count [a-seq]
  :-)

(defn my-reverse [a-seq]
  [:-])

(defn min-max-element [a-seq]
  [:-])

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