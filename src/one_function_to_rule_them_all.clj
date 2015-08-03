(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (cond
   (empty? a-seq) ""
   :else (reduce (fn [eka toka] (str eka " " toka)) a-seq)))

;kokeilin kayttaa ensimmaista kertaa anonyymia funktiota, siistia!
(defn my-interpose [x a-seq]
  (cond
   (empty? a-seq) a-seq
   :else (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [seed seq1] (inc seed)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [new-seq s1] (conj new-seq s1)) () a-seq))

(defn min-max-element [a-seq]
  (reduce
   (fn
     [[mini maxi] new]
     (cond
      (< new mini) [new maxi]
      (> new maxi) [mini new]
      :else [mini maxi]))
     [(first a-seq) (first a-seq)]
   a-seq))

(defn insert [sorted-seq n]
  (loop [uusi-seq [] vanha-seq sorted-seq]
    (cond
     (empty? vanha-seq) (concat uusi-seq [n])
     (<= n (first vanha-seq)) (concat uusi-seq [n] vanha-seq)
     :else (recur (concat uusi-seq [(first vanha-seq)]) (rest vanha-seq)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce
   #(if (contains? %1 %2)
      (disj %1 %2)
      (conj %1 %2))
   #{}
   a-seq))

;arity overload!
(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (my-count more)))

;such elegance!
(defn my-*
  ([] 1)
  ([& more] (reduce #(* %1 %2) 1 more)))

(defn pred-and
  ([] (fn [x] true))
  ([fx & more] (fn [x] (reduce #(and %1 (%2 x)) (fx x) more))))

(defn my-map [f a-seq]
  [:-])
