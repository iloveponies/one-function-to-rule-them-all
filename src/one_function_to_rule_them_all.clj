(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [a b] (conj (conj a x) b)) (vector (first a-seq)) (rest a-seq))))

; count used but only as a variable name!
(defn my-count [a-seq]
  (let [counter (fn [count e] (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [result element] (cons element result)) '() a-seq))) 

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    '[0 0]
    (let [minmax (fn [[oldmin oldmax] element]
      (vector (min oldmin element) (max oldmax element)))]
        (reduce minmax (vector (first a-seq) (first a-seq)) a-seq))))

(defn insert [sorted-seq n]
  (loop [acc [] coll sorted-seq]
    (if (or (empty? coll) (<= n (first coll)))
      (concat acc (vector n) coll)
      (recur (conj acc (first coll)) (rest coll)))))

(defn insertion-sort [a-seq]
  (reduce (fn [result element] (insert result element)) '() a-seq))

(defn parity [a-seq]
  (let [toggler (fn [result element]
                  (if (contains? result element)
		    (disj result element)
		    (conj result element)))]
    (reduce toggler '#{} a-seq)))

(defn minus
      ([x] (- x))
      ([x y] (- x y)))

(defn count-params [& more]
      (reduce (fn [result element] (+ result 1)) 0 more))

(defn my-*
      ([] 1)
      ([x] x)
      ([x & more]
      	  (reduce * x more))) 

(defn pred-and
  ([& more]
    (fn [elem]
      (reduce (fn [result x] (and result (x elem))) true more))))

(defn first-each [& more]
      (reduce (fn [result element] (cons (first element) result)) '() more))

(defn f-first-each [f & more]
      (apply f (reverse (reduce (fn [result element] (cons (first element) result)) '() more))))
;
; 1. take the first element of all params, create vector, apply f
; 2. assign result as the next element of the result
; 3. take the rest of all params and goto step 1
;

(defn rest-each [& more]
      (reverse (reduce (fn [result x] (cons (apply vector (rest x)) result)) '() more)))

(defn rest-each-plus [f & more]
      (apply map f (reverse (reduce (fn [result x] (cons (apply vector (rest x)) result)) '() more))))

(defn my-map [f & more]
  (if
    (not (not-any? empty? more)) ()
    (cons
      (apply f
      	     (reverse (reduce (fn [result x] (cons (first x) result)) '() more)))
      (apply my-map f
      	     (reverse (reduce (fn [result x] (cons (apply vector (rest x)) result)) '() more)))
	     )))

