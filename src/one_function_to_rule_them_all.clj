(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
      (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (reverse 
   (loop [input a-seq
          output '()]
     (cond (empty? input) output
           (empty? (rest input)) (conj output (first input))
           :else (recur (rest input) 
                        (conj output (first input) x))))))

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
