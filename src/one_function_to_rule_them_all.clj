(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (reduce str "" (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (seq (reduce (fn [first second]
                 (if (empty? first) (conj first second)
                     (conj first x second))) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [first second]
            (cons second first)) [] a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (let [place (count (filter #(> n %) sorted-seq))
        [before after] (split-at place sorted-seq)]
    (concat before (cons n after))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

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
