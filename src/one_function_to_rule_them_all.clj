(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))


(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (cond
   (empty? a-seq) '()
   (== 1 (count a-seq)) (seq a-seq)
   :else (flatten (reduce (fn [a b] (conj [a x] b)) a-seq))))


(defn my-count [a-seq]
  (let [counter (fn [n _]
                  (inc n))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    '()
    (flatten (reduce (fn [a b] (conj [b] a)) a-seq))))

(defn min-max-element [a-seq]
  (let [minmax (fn [result e]
                 (cond
                  (< e (get result 0)) (conj [e] (get result 1))
                  (> e (get result 1)) (conj [(get result 0)] e)
                  :else result))]
    (reduce minmax [(first a-seq) (first a-seq)] a-seq)))


(defn insert [sorted-seq n]
   (if (or (empty? sorted-seq) (< n (first sorted-seq)))
   (cons n sorted-seq)
   (cons (first sorted-seq) (insert (rest sorted-seq) n))))

; alternative solution, java hangs, dunno why

;(defn insert [sorted-seq n]
;  (loop [new-seq '()
;         data sorted-seq]
;    (cond
;     (empty? data) (conj new-seq n)
;     (< n (first data)) (concat new-seq (cons n data))
;     :else (recur (conj new-seq (first data)) (rest data)))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [helper (fn [result curr]
                 (if (contains? result curr)
                   (disj result curr)
                   (conj result curr)))]
    (reduce helper #{} a-seq)))


(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (reduce (fn [n _] (inc n)) 0 x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))


(defn pred-and
  ([] (fn [x] true))
  ([pred] pred)
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more]
   (reduce pred-and (pred-and pred1 pred2) more)))


; enter my-map helpers

(defn cons-by-index [index seqs]
  (if (empty? seqs)
   '()
    (cons (first (drop index (first seqs))) (cons-by-index index (rest seqs)))))

(defn cons-by-index-all [seqs]
  (let [length (count seqs)]
    (loop [index 0
           result '()]
      (if (== index length)
        (reverse result)
        (recur (inc index) (cons (cons-by-index index seqs) result))))))

(defn my-map
  ([f a-seq]
   (let [helper (fn [result curr]
                  (if (seq? curr)
                    (conj result (apply f curr))
                    (conj result (f curr))))]
     (reverse (reduce helper '() a-seq))))
  ([f a-seq & more]
   (my-map f (cons-by-index-all (cons a-seq more)))))





