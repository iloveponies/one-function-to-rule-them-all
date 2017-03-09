(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (let [spaced-seq (interpose " " a-seq)]
    (reduce str spaced-seq)))

(defn my-interpose [x a-seq]
  (reduce (fn [acc elem] (if (empty? acc) (conj acc elem) (conj acc x elem))) [] a-seq))

(defn my-count [a-seq]
  (reduce (fn [x elem] (if-not (nil? elem) (inc x))) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [base elem] (cons elem base)) '() a-seq))

(defn min-max-element [a-seq]
  (let [min-max (fn [base elem] 
                (let [[mi ma] base]
				  [(min mi elem) (max ma elem)]))]
	(if-not (empty? a-seq)
	  (reduce min-max [(first a-seq) (first a-seq)] (rest a-seq))
	  [])))

(defn insert [sorted-seq n]
  (let [smaller (take-while (fn [seq] (<= seq n)) sorted-seq)
        greater (drop-while (fn [seq] (<= seq n)) sorted-seq)]
	(if-not (empty? sorted-seq)
	  (concat smaller [n] greater)
	  [n])))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [set elem] (if-not (empty? set)
	                      (if(contains? set elem)
                           (disj set elem)
	                       (conj set elem))
						  #{elem})) #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x y] 2)
  ([x y & more] (reduce (fn [acc elem] (inc acc)) 2 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and 
  ([] (constantly true))
  ([p] p)
  ([p1 p2] (fn [elem] (and (p1 elem) (p2 elem))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))
(cons seq1 more)

(defn my-map [f & seq]
 (if (every? empty? seq)
   []
   (let [fsts (map first seq)
      tsts (map rest seq)]
       (cons (apply f fsts) (apply my-map f tsts)))))