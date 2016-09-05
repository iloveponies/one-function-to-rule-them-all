(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
	(reduce concat () a-seq)
)

(defn str-cat [a-seq]
	(reduce str (interpose " " a-seq))
)

(defn my-interpose [x a-seq]
	(rest (reduce #(conj %1 x %2) [] a-seq))
)

(defn my-count [a-seq]
	(reduce (fn [curCount _] (inc curCount)) 0 a-seq)
)

(defn my-reverse [a-seq]
	(reduce (fn [reversed restseq] (cons restseq reversed)) [] a-seq)
)

(defn min-max-element [a-seq]
	(reduce (fn [[minval maxval] comparedval] [(min minval comparedval) (max maxval comparedval)]) [(first a-seq) (first a-seq)] a-seq)
)

(defn insert [sorted-seq n]
	(loop [head [] tail sorted-seq] 
		(let [tailstart (first tail)]
			(if (or (empty? tail) (<= n tailstart))
				(concat (conj head n) tail)
				(recur (conj head tailstart) (rest tail))
			)
		)
	)
)

(defn insertion-sort [a-seq]
	(reduce insert [] a-seq)
)

(defn parity [a-seq]
	(let [toggle (fn [x y]
					(if (contains? x y)
						(disj x y)
						(conj x y)
					)
				)
			]
		(reduce toggle #{} a-seq)		
	)	
)

(defn minus 
	([x] (- x x x))
	([x y] (- x y))
)

(defn count-params 
	([] 0)
	([& more]
		(reduce (fn [curcount params] (+ curcount 1)) 0 more)
	)
)

(defn my-* 
	([] 1)
	([x] x)
	([x y] (* x y))
	([x y & more] (reduce * (* x y) more))
)

(defn pred-and
	([] (fn [p] true))
	([p] p)
	([p1 p2] (fn [p] (and (p1 p)(p2 p))))
	([p1 p2 & more] (reduce (fn [x y] (pred-and x y)) (pred-and p1 p2) more))
)

(defn my-map [f a-seq]
  [:-])