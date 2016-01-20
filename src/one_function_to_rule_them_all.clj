(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (let [insrt (fn [b-seq el]
		(conj b-seq x el))]
    (reduce insrt (conj [] (first a-seq)) (rest a-seq)))))

(defn my-count [a-seq]
  (let [cnt (fn [i el]
		(inc i))]
    (reduce cnt 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rv (fn [b-seq el]
	     (cons el b-seq))]
    (reduce rv [] a-seq)))

(defn min-max-element [a-seq]
  (let [e (first a-seq)
	res (conj [] e e)
	mim (fn [m-p el] 
	     (if (< el (first m-p))
		(assoc m-p 0 el)
		(if (> el (second m-p))
		  (assoc m-p 1 el)
		  m-p)))]
    (reduce mim res (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [seq-1 []
	 seq-2 sorted-seq]
    (if (empty? seq-2)
      (conj seq-1 n)
      (let [e  (first seq-2)]
        (if (< n e)
	  (concat (conj seq-1 n) seq-2)
	  (recur (conj seq-1 e) (rest seq-2)))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set el]
		  (if (contains? a-set el)
		    (disj a-set el)
    		    (conj a-set el)))]
    (reduce toggle #{} a-seq)))

(defn minus 
	([x] (* -1 x))
  	([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-* [& x]
  (reduce * 1 x))

(defn pred-and [& pr]
  (fn [x] 
    (let [p-and (fn [acc el] 
		  (and acc (el x)))]
      (reduce p-and 1 pr))))

(defn my-map [f a-seq]
  [:-])
