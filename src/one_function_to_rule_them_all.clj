(ns one-function-to-rule-them-all)
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (seq a-seq)
    (reduce #(str %1 " " %2) a-seq)
    ""))

(defn my-interpose [x a-seq]
  (if (seq a-seq)
    (rest (reduce #(conj %1 x %2) [] a-seq))
    ()))

(defn my-count [a-seq]
  (let [count (fn [count e]
                (inc count))]
    (reduce count 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev-acc (fn [acc e]
                  (conj acc e))]
    (reduce rev-acc '() a-seq)))

(defn min-max-element [a-seq]
  (let [[f & s] a-seq]
    (cond
     (not f) []
     (not s) [f f]
     :else
     (let [minmax (fn [acc e]
                    [(min (first acc) e) (max (second acc) e)])]
       (reduce minmax [(first a-seq) (second a-seq)] a-seq)))))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (cons n sorted-seq)
    (let [[x & xs] sorted-seq] 
      (cond
       (< n x) (cons n sorted-seq)
       :else (cons x (insert xs n))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(insertion-sort [2 5 3 1]) ;=> (1 2 3 5)
(insertion-sort [1 2])     ;=> (1 2)

(defn toggle [s e]
  (if (contains? s e)
    (disj s e)
    (conj s e)))

(defn parity [a-seq]
  (let [odd-accum (fn [accum e]
                    (toggle accum e))]
    (reduce odd-accum #{} a-seq)))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(minus 2)   ;=> -2
(minus 4 3) ;=> 1

(defn count-params
  ([& more] (count more)))

(count-params)            ;=> 0
(count-params :a)         ;=> 1
(count-params :a 1 :b :c) ;=> 4

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
     (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [s] true))
  ([pred] pred)
  ([pred1 pred2] (fn [s] (and (pred1 s) (pred2 s))))
  ([pred1 pred2 & more]
     (reduce pred-and (pred-and pred1 pred2) more)))

(filter (pred-and) [1 0 -2])                    ;=> (1 0 -2)
(filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> (1 7)
(filter (pred-and number? integer? pos? even?)
        [1 0 4 -2 :a 7 "a" 2])                    ;=> (0 2)

(defn my-map
  [f & a-seqs]
  (if (some empty? a-seqs)
    `()
    (cons (apply f (map first a-seqs))
          (apply (partial my-map f) (map rest a-seqs)))))

(my-map + [1 1 1] [2 2 2])              ;=> (3 3 3)
(my-map inc [1 2 3 4])                  ;=> (2 3 4 5)
(my-map + [1 1 1] [1 1 1] [1 1 1])      ;=> (3 3 3)
(my-map vector [1 2 3] [1 2 3] [1 2 3]) ;=> ((1 1 1) (2 2 2) (3 3 3))
