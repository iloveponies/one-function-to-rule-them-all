(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (if (empty? a-seq) ()
      (seq (reduce concat a-seq))))

(concat-elements [])            ;=> ()
(concat-elements [[1 2]])       ;=> (1 2)
(concat-elements [[1 2] [3 4]]) ;=> (1 2 3 4)

(defn str-cat [a-seq]
  (reduce #(str %1 " " %2)
          ""
          a-seq))
            
(str-cat ["I" "am" "Legend"])  ;=> "I am Legend"
(str-cat ["I" "am" "back"])    ;=> "I am back"
(str-cat ["more" " " "space"]) ;=> "more   space"
(str-cat [])                   ;=> ""

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) 
                [] 
                a-seq)))

(my-interpose 0 [1 2 3])               ;=> (1 0 2 0 3)
(my-interpose "," ["I" "me" "myself"]) ;=> ("I" "," "me" "," "myself")
(my-interpose :a [1])                  ;=> (1)
(my-interpose :a [])                   ;=> ()

(defn my-count [a-seq]
  (let [counter (fn [count x]
                  (inc count))]
    (reduce counter 0 a-seq)))

(my-count [])      ;=> 0
(my-count [1 2 3]) ;=> 3
(my-count [1])     ;=> 1

(defn my-reverse [a-seq]
  (reduce (fn [x y] (conj x y)) () a-seq))

(my-reverse [1 2 3]) ;=> (3 2 1)
(my-reverse [1 2])   ;=> (2 1)
(my-reverse [])      ;=> ()

(defn min-max-element [a-seq]
  (reduce (fn [[x y] z]
            [(min x z)
             (max y z)]) [(first a-seq) (first a-seq)] a-seq))

(min-max-element [2 7 3 15 4]) ;=> [2 15]
(min-max-element [1 2 3 4])    ;=> [1 4]
(min-max-element [1])          ;=> [1 1]

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) (cons n ())
    (> (first sorted-seq) n) (cons n sorted-seq)
    :else
    (cons(first sorted-seq) (insert (rest sorted-seq) n))))

(insert [] 2)      ;=> (2)
(insert [1 3 4] 2) ;=> (1 2 3 4)
(insert [1] 2)     ;=> (1 2)

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(insertion-sort [2 5 3 1]) ;=> (1 2 3 5)
(insertion-sort [1 2])     ;=> (1 2)

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(parity [:a :b :c])    ;=> #{:a :b :c}
(parity [:a :a :b :b]) ;=> #{}
(parity [1 2 3 1])     ;=> #{2 3}

(defn minus
  ([x] (* x -1))
  ([x y] (- x y)))

(minus 2)   ;=> -2
(minus 4 3) ;=> 1

(defn count-params 
  ([] 0)
  ([x] 1)
  ([x & more] (reduce (fn [x y](inc x)) 0 more)))
  

(count-params)            ;=> 0
(count-params :a)         ;=> 1
(count-params :a 1 :b :c) 

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * 1 more)))


(my-*)           ;=> 1
(my-* 4 3)       ;=> 12
(my-* 1 2 3 4 5) ;=> 120


(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
