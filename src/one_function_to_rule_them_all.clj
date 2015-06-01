(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) 
  	""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) 
  	[]
    (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (if (empty? a-seq)
    0
    (reduce (fn [c _] (inc c)) 0 a-seq)))

(defn my-reverse [a-seq]
	(if (empty? a-seq)
		'()
		(reduce (fn [acc x] (cons x acc)) '() a-seq)))

(defn min-max-element [a-seq]
  (reduce (fn [[x-min x-max] x]
  	[(min x-min x) (max x-max x)]) [(first a-seq) (first a-seq)] (rest a-seq)))

(defn insert [sorted-seq n]
  [:-])

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  (let [helper (fn [a-set elem]
    (if (contains? a-set elem) 
    	(disj a-set elem)
    	(conj a-set elem)))]
    (reduce helper #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])