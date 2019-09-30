(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq) )

(defn str-cat [a-seq]
  (if (empty? a-seq)
	   ""
	  (reduce (fn [str1 str2] (str str1 " " str2)) a-seq) ) )

(defn my-interpose [x a-seq]
  (if (<= (count a-seq) 1)
      a-seq
	  (cons (first a-seq) (cons x (my-interpose x (rest a-seq)))) ) )

(defn my-count [a-seq]
  (reduce (fn [count x] (inc count)) 0 a-seq) )

(defn my-reverse [a-seq]
  (reduce (fn [res elem] (cons elem res)) '() a-seq) )

(defn min-max-element [a-seq]
  (if (empty? a-seq)
      '[]
	  (reduce (fn [[min max] elem]
                (cond
				  (< elem min) [elem max]
				  (> elem max) [min elem]
				  :else		   [min max] ) )
			  [(first a-seq) (first a-seq)]
			  (rest a-seq) ) ) )

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) (cons n '())
	(< n (first sorted-seq)) (cons n sorted-seq)
	:else (cons (first sorted-seq) (insert (rest sorted-seq) n)) ) )

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq) )

; A helper function for the next problem (parity) 
(defn toggle [a-set elem]
  ((if (contains? a-set elem) disj conj) a-set elem) )
  
(defn parity [a-seq]
  (reduce toggle '#{}  a-seq) )

(defn minus
  ([x] (- x))
  ([x y] (-  x y)) )

(defn count-params [& more]
  (my-count more) )

(defn my-*
  ([] 1)
  ([x] x)
  ([x & more] (reduce * x more)) )

(defn pred-and
  ([]         (fn [x] x))
  ([p]         p)
  ([p & more] (fn [x] (if (p x) ((apply pred-and more) x) false))) )

(defn my-map [f & a-seq]
  :-)