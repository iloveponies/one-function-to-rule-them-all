(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(concat-elements [])            ;=> ()
(concat-elements [[1 2]])       ;=> (1 2)
(concat-elements [[1 2] [3 4]]) ;=> (1 2 3 4)

(defn str-cat [a-seq]
  (if
    (empty? a-seq) ""
    (reduce (fn [x y] (str x " " y)) a-seq))
  )


(str-cat ["I" "am" "Legend"])  ;=> "I am Legend"
(str-cat ["I" "am" "back"])    ;=> "I am back"
(str-cat ["more" " " "space"]) ;=> "more   space"
(str-cat ["one"])                   ;=> ""
(str-cat [])                   ;=> ""

(defn my-interpose [x [f & r :as a-seq]]
  (if (empty? a-seq)
    '()
    (reduce (fn [a b] (conj a x b)) [f] r)))

(let [a-seq [1 2 3]]
  (conj [(first a-seq)] (rest a-seq) :a))

(my-interpose 0 [1 2 3])               ;=> (1 0 2 0 3)
(my-interpose "," ["I" "me" "myself"]) ;=> ("I" "," "me" "," "myself")
(my-interpose :a [1])                  ;=> (1)
(my-interpose :a [])                   ;=> ()

(defn my-count [a-seq]
  (reduce
    (fn [count x]
      (inc count))
    0
    a-seq))

(my-count '())      ;=> 0
(my-count [])      ;=> 0
(my-count [1 2 3]) ;=> 3
(my-count [1])     ;=> 1

(defn my-reverse [a-seq]
  (reduce
    (fn [acc x]
      (conj acc x)) '() a-seq))

(my-reverse [1 2 3]) ;=> (3 2 1)
(my-reverse [1 2])   ;=> (2 1)
(my-reverse [])      ;=> ()

(defn min-max-element [a-seq]
  (reduce (fn [v x]
            (cond
              (empty? v)
                [x x]
              (< x (first v))
                (assoc v 0 x)
              (< (second v) x)
                (assoc v 1 x)
              :else
                v)) [] a-seq))

(min-max-element [2 7 3 15 4]) ;=> [2 15]
(min-max-element [1 2 3 4])    ;=> [1 4]
(min-max-element [1])          ;=> [1 1]
(min-max-element [])          ;=> [1 1]

(defn insert [sorted-seq n]
  (cond
    (or (empty? sorted-seq) (< n (first sorted-seq)))
      (cons n sorted-seq)
    :else
      (cons (first sorted-seq) (insert (rest sorted-seq) n)))
  )

(insert [] 2)      ;=> (2)
(insert [1 3 4] 2) ;=> (1 2 3 4)
(insert [1] 2)     ;=> (1 2)

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(insertion-sort [2 5 3 1]) ;=> (1 2 3 5)
(insertion-sort [1 2])     ;=> (1 2)

(defn parity [a-seq]
  (let [toggle (fn [s x] (if (contains? s x) (disj s x) (conj s x)))]
    (reduce toggle #{} a-seq)))

(parity [:a :b :c])    ;=> #{:a :b :c}
(parity [:a :a :b :b]) ;=> #{}
(parity [1 2 3 1])     ;=> #{2 3}

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(minus 2)   ;=> -2
(minus 4 3) ;=> 1
(minus 1)
(minus 3 1)

(defn count-params [& more]
  (count more))

(count-params)            ;=> 0
(count-params :a)         ;=> 1
(count-params :a 1 :b :c) ;=> 4

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y z & more] (reduce * (* x y z) more)))

(my-*)           ;=> 1
(my-* 4 3)       ;=> 12
(my-* 1 2 3 4 5) ;=> 120

(defn pred-and
  ([] (fn [x] true))
  ([f] f)
  ([f1 f2 & more] (fn [x] (and (f1 x) (f2 x) (every? (fn [y] (y x)) more)))))

(filter (pred-and) [1 0 -2])            ;=> (1 0 -2)
(filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> (1 7)
(filter (pred-and number? integer? pos? even?) [1 0 -2 :a 7 "a" 2])                    ;=> (0 2)

(def ex [[1 2 3] [4 5 6] [7 8 9] [10 11]])
(def ex1 [[1 1 1] [1 1 1] [1 1 1]])

(defn reassemble [colls]
  (if (every? identity (map seq colls))
    (cons (map first colls) (reassemble (map rest colls)))
    []))

(reassemble ex)
(reassemble ex1)

(defn my-map
  ([f coll]
   (let [s (seq coll)]
     (if (nil? s)
       []
       (cons (f (first s)) (my-map f (rest s))))))
  ([f c1 c2]
   (let [s1 (seq c1) s2 (seq c2)]
     (if (and s1 s2)
       (cons (f (first s1) (first s2)) (my-map f (rest s1) (rest s2)))
       [])))
  ([f c1 c2 & colls]
   (let [seqs (my-map seq (conj colls c2 c1 ))
         rseqs (reassemble seqs)
         ]

     (my-map (fn [x] (apply f x)) rseqs)))
     )

(my-map + [1 2 3 4] [1 2 3] [5 6 7] [8 9 10])

(def rs (reassemble [[1 2 3 4] [1 2 3] [1 2] [1]]))

(map (fn [x] (apply + x)) (reassemble [[1 2 3 4] [1 2 3] [1 2] [1 2]]))

(conj [[1 2 3 4]] [1 2 3] [1 2])

(map + (my-map first ex))

(my-map - [1 1 2 1])
(my-map + [1 1 2 1] [1 2 1])
(my-map + [1 2 3 4] [1 2 3] [1 2] [1])
(map + [1 2 3 4] [1 2 3] [1 2])

(map + [1 2 3] [1 2 3] [2])
(map + [1 1 2 1])
(map - [1 1 2 1])
(map + [1 1 1] [1 1 1] [1 1 1])
(map vector [1 2 3] [1 2 3] [1 2 3])

(vector [1 2 3])

(map + [3] [1 2 3 4 5] [2 3 4 5 6])                  ;=> (2 3 4 5)
(my-map + [1 1 1])      ;=> (3 3 3)
(my-map + [1 1 1] [2 2 2])      ;=> (3 3 3)
(my-map + [1 1 1] [1 1 1] [1 1 1])      ;=> (3 3 3)
(my-map vector [1 2 3 4] [1 2 3 4] [1 2 3 4]) ;=> ((1 1 1) (2 2 2) (3 3 3))
