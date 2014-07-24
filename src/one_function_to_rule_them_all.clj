(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (sequence (reduce #(conj %1 x %2) (vector (first a-seq)) (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn[n elem]
                 (inc n))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [add-before (fn[a-seq elem]
            (cons elem a-seq))]
    (reduce add-before '() a-seq)))

(defn min-max-element [a-seq]
  (let [update-min-max (fn[[mn mx] current]
                         [(min mn current) (max mx current)])
        fst (first a-seq)]
    (reduce update-min-max [fst fst] (rest a-seq))))

(defn insert [sorted-seq n]
  (concat
    (filter #(< % n) sorted-seq)
    (list n)
    (filter #(>= % n) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn[a-set elem]
                 (if (a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(conj [1] 2)

(nth '(1 2 3) 1)

(defn pred-and
  ([]      (fn[v] true))
  ([x?]    x?)
  ([x? y?] (fn[v] (and (x? v) (y? v))))
  ([x? y? & more?] (fn[v] (reduce #(and %1 (%2 v)) ((pred-and x? y?) v) more?))))

(defn get-nth [a-seq n]
  (let [helper (fn[result a-seq]
                 (if (<= (count a-seq) n)
                   nil
                   (cons (nth a-seq n) result)))]
    (reduce helper '() a-seq)))

(defn my-map2 [f & more-seq]
  (reduce #(concat %1 ()) '() more-seq))

(get-nth '((1 2 3 4) (1 2 3 4) (1 2 3 4)) 3)

(defn temp [a-seq]
  (let [crazy (fn[[remaining result] inner]
                (if (empty? inner)
                  [])
                [(conj remaining (rest inner)) (conj result (first inner))])
        [remaining result] (reduce crazy [[] []] a-seq)]
    (cons (sequence result) (temp remaining))))

(temp '((1 2 3) (1 2 3) (1 2 3)))
(temp '((1) (1) (1)))


(defn my-map
  ([f a-seq] (sequence (reduce #(conj %1 (f %2)) [] a-seq)))
  ([f a-seq b-seq]
    (loop [remain-a-seq a-seq remain-b-seq b-seq result []]
      (if (or (empty? remain-a-seq) (empty? remain-b-seq))
        (sequence result)
        (recur (rest remain-a-seq) (rest remain-b-seq) (conj result (f (first remain-a-seq) (first remain-b-seq)))))))
  ([f a-seq b-seq & more]
    (reduce #(my-map f %1 %2) (my-map f a-seq b-seq) more)))


(my-map inc [1 2 3 4])                  ;=> (2 3 4 5)
(my-map + [1 1 1] [1 1 1])
(my-map + [1 1 1] [1 1 1] [1 1 1])      ;=> (3 3 3)
(my-map vector [1 2 3] [1 2 3] [1 2 3]) ;=> ((1 1 1) (2 2 2) (3 3 3))

(my-map vector [1 2 3] [1 2 3])
