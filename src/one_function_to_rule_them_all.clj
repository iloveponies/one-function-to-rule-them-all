(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (cond
   (empty? a-seq) ""
   :else (loop [seq1 a-seq] (reduce
                             (fn [x y] (str x " " y ))
                              seq1))))

(str-cat ["I" "am" "Legend"])  ;=> "I am Legend"
(str-cat ["I" "am" "back"])    ;=> "I am back"
(str-cat ["more" " " "space"]) ;=> "more   space"
(str-cat [])

(defn my-interpose [x a-seq]
(cond
   (empty? a-seq) ()
   :else (loop [seq1 (vec a-seq)]
           (reverse (reduce
                     (fn [a b]
                       (cond
                        (empty? a) (conj a b)
                        :else (conj a x b)))
                     () seq1)))))

(my-interpose 0 [1 2 3])             ;=> (1 0 2 0 3)
(my-interpose "," ["I" "me" "myself"]) ;=> ("I" "," "me" "," "myself")
(my-interpose :a [1])                  ;=> (1)
(my-interpose :a [])                   ;=> ()

(defn my-count [a-seq]
  (reduce (fn [x y] (+ x 1)) 0 a-seq))

(my-count [])      ;=> 0
(my-count [1 2 3]) ;=> 3
(my-count [1])     ;=> 1

(defn my-reverse [a-seq]
  (into () a-seq)); (reduce (fn [x y] (cons y x)) [] a-seq)))

(my-reverse [1 2 3]) ;=> (3 2 1)
(my-reverse [1 2])   ;=> (2 1)
(my-reverse [])      ;=> ()

(defn min-max-element [a-seq]
  (reduce (fn [x y] [(min (first x) y) (max (second x) y)])
          [(first a-seq) (first a-seq)] a-seq))

(min-max-element [2 7 3 15 4]) ;=> [2 15]
(min-max-element [1 2 3 4])    ;=> [1 4]
(min-max-element [1])          ;=> [1 1]

(defn insert [sorted-seq n]
  (let [v (vec sorted-seq)]
    (cond
     (empty? v) (list n)
     (< n (first v)) (cons n v)
     :else (conj  (insert (rest v) n) (first v)))))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(insertion-sort [2 5 3 1]) ;=> (1 2 3 5)
(insertion-sort [1 2])     ;=> (1 2)

(defn parity [a-seq]
  (reduce (fn [x y] (if (contains? x y) (disj x y) (conj x y))) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x y] 2)
  ([x y & more]
    (reduce (fn [x y] (+ x 1)) 2 more)))

(count-params)            ;=> 0
(count-params :a)         ;=> 1
(count-params :a 1 :b :c) ;=> 4

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce (fn [a b] (* a b)) (* x y) more)))

(my-*)           ;=> 1
(my-* 4 3)       ;=> 12
(my-* 1 2 3 4 5) ;=> 120

(defn pred-and
  ([] (fn [val] true))
  ([x] (fn [val] (x val)))
  ([x y] (fn [val] (and (x val) (y val))))
  ([x y & more] (fn [val]
                  (and (x val) (y val) (every? (fn [func] (func val)) more)))))


                  ;(reduce ever
                   ;(fn [a b] (pred-and a b))
                   ;(pred-and x y)
                   ;more))))



(defn my-map
  ([f a-seq] (map f a-seq))
  ([f a-seq & more] (map f a-seq more)))