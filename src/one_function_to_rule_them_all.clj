(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [z a] (str z " " a)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [z a] (concat z [x a])) [(first a-seq)] a-seq)))

(defn my-count [a-seq]
  (reduce inc 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [z a] (cons a z)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[mn mx] a] 
            [(min mn a) (max mx a)])
    [(first a-seq) (first a-seq)] (rest a-seq))) 

(defn insert [sorted-seq n]
  (loop [acc []
         sq sorted-seq]
    (cond
      (empty? sq) (reverse (cons n acc))
      (>= (first sq) n) (concat (reverse acc) (cons n sq))
      :else (recur (cons (first sq) acc) (rest sq)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

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
