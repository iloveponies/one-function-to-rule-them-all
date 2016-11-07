(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
	(reduce concat '() a-seq)
)

(defn str-cat [a-seq]
	(reduce str "" (reduce
		(fn [x z] 
			(concat x (if (empty? x) "" " ") z)
		)
		"" a-seq
	))
)

(defn my-interpose [x a-seq]
	(into [] (reverse (reduce (fn [old cur]
		(if (empty? old) (conj old cur) (conj (conj old x) cur))
	) '() a-seq)))
)

(defn my-count [a-seq]
	(reduce (fn [n c] 
		(inc n)
	) 0 a-seq)
)

(defn my-reverse [a-seq]
	(reduce (fn [old cur] 
		(conj old cur)
	) '() a-seq)
)

(defn min-max-element [a-seq]
	(reduce (fn [[Xmin Xmax] cur] 
		[(if (nil? Xmin) cur (min Xmin cur)) (if (nil? Xmax) cur (max Xmax cur))]
	) [nil nil] a-seq)
)

(defn insert [sorted-seq n]
	(reverse (loop [cum '() r sorted-seq]
		(cond
			(empty? r)
				(conj cum n)
			(> (first r) n)
				(apply conj cum n r)
			:else
				(recur (conj cum (first r)) (rest r)))
	))
)

(defn insertion-sort [a-seq]
	 (reduce insert '() a-seq)
)

(defn parity [a-seq]
	(reduce
		(fn [old cur]
			(cond
				(contains? old cur)
					(disj old cur)
				:else
					(conj old cur)
			)
		) #{} a-seq
	)
)

(defn minus
	([x] (- 0 x))
	([x y] (- x y))
)

(defn count-params 
	([] 0)
	([& more] (count more))
)

(defn my-* 
	([] 1)
	([x] x)
	([x y & more] (apply * x y more))
)

(defn pred-and
	([] (fn [x] true))
	([& more] (fn [x] 
		(every? identity (map (fn [y] (y x)) more))
	))
)

(defn my-map 
	([f a-seq]
		(if (empty? a-seq)
			'()
			(cons (f (first a-seq)) (my-map f (rest a-seq)))
		)
	)
	([f a-seq & more]
		(if (some empty? (cons a-seq more))
			'()
			(cons (apply f (my-map first (cons a-seq more))) (apply my-map f (my-map rest (cons a-seq more))))
	))
)