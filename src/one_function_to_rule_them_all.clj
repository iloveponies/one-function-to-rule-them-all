(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq)
  )

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq))
  )

(defn my-interpose [x a-seq]
  (apply concat (interpose [x] (partition 1 a-seq)))
  )

(defn my-count [a-seq]
  (reduce (fn [res cur] (inc res)) 0 a-seq)
  )

(defn my-reverse [a-seq]
  (reduce (fn [res seq] (cons seq res)) [] a-seq)
  )

(defn min-max-element [a-seq]
  (let [helper (fn [[lo hi] item]
                 (cond (< item lo) [item hi]
                 (> item hi) [lo item]
                 :else [lo hi]))]
    (reduce helper [(first a-seq) (first a-seq)] (rest a-seq)))
  )

(defn insert [sorted-seq n]
  (sort (conj sorted-seq n))
  )

(defn insertion-sort [a-seq]
  (reduce insert () a-seq)
  )

(defn parity [a-seq]
  (let [helper (fn [set elem]
                 (if (contains?  set elem)
                   (disj set elem)
                   (conj set elem)))]
    (reduce helper #{} a-seq))
  )

(defn minus
  ([x] (- x))
  ([x y] (- x y))
  )

(defn count-params ([& x] (count x))
  )

(defn my-* ([& x] (apply * x))
  )

(defn pred-and
  ([] (fn [n] true))
  ([pred] pred)
  ([pred1 pred2] (fn [n] (and (pred1 n) (pred2 n))))
  ([pred1 pred2 & params] (reduce pred-and (pred-and pred1 pred2) params))
  )

(defn my-map
  ([f a-seq] (let [helper (fn [newseq elem] (concat newseq (list (f elem))))] (reduce helper '() a-seq)))
  ([f a-seq & params] (if (empty? a-seq) '()
                        (cons (apply f (my-map first (cons a-seq params)))
                              (apply my-map f (my-map rest (cons a-seq params))))))

  )

; :-)]}
