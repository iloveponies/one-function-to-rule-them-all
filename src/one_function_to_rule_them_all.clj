(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq)
  )

(concat-elements [])            ;=> ()
(concat-elements [[1 2]])       ;=> (1 2)$
(concat-elements [[1 2] [3 4]]) ;=> (1 2 3 4)

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))
    )
  )

(str-cat ["I" "am" "Legend"])  ;=> "I am Legend"
(str-cat ["I" "am" "back"])    ;=> "I am back"
(str-cat ["more" " " "space"]) ;=> "more   space"
(str-cat [])                   ;=> ""

(defn my-interpose [x a-seq]
 (if (empty? a-seq)
   ()
   (reduce conj (interpose x a-seq) nil)
   )
  )

(my-interpose 0 [1 2 3])               ;=> (1 0 2 0 3)
(my-interpose "," ["I" "me" "myself"]) ;=> ("I" "," "me" "," "myself")
(my-interpose :a [1])                  ;=> (1)
(my-interpose :a [])                   ;=> ()

(defn my-count [a-seq]
  (let [counter (fn [a a-seq]
                  (inc a)
                  )]
    (reduce counter 0 a-seq)
    )
  )

(my-count [])      ;=> 0
(my-count [1 2 3]) ;=> 3
(my-count [1])     ;=> 1

(defn my-reverse [a-seq]
  (reduce conj '() a-seq)
  )

(my-reverse [1 2 3]) ;=> (3 2 1)
(my-reverse [1 2])   ;=> (2 1)
(my-reverse [])      ;=> ()

(defn min-max-element [a-seq]
  [(apply min a-seq) (apply max a-seq)]
  )

(min-max-element [2 7 3 15 4]) ;=> [2 15]
(min-max-element [1 2 3 4])    ;=> [1 4]
(min-max-element [1])          ;=> [1 1]

(defn insert [sorted-seq n]
  (let [ins (fn [bseq n]
              (if (< (first bseq) n)
                (cons (first bseq) (insert (rest bseq) n))
                (cons n bseq)
                )
              )]
    (if (empty? sorted-seq)
      [n]
      (ins sorted-seq n)
      )
    )
  )

(insert [] 2)      ;=> (2)
(insert [1 3 4] 2) ;=> (1 2 3 4)
(insert [1] 2)     ;=> (1 2)

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq)
  )

(insertion-sort [2 5 3 1]) ;=> (1 2 3 5)
(insertion-sort [1 2])     ;=> (1 2)

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn parity [a-seq]
  (reduce toggle #{} a-seq)
  )

(parity [:a :b :c])    ;=> #{:a :b :c}
(parity [:a :a :b :b]) ;=> #{}
(parity [1 2 3 1])     ;=> #{2 3}

(defn minus
  ([x] (- x))
  ([x y] (- x y))
  )

(minus 2)   ;=> -2
(minus 4 3) ;=> 1

(defn count-params
  ([& m] (count m))
  )

(count-params)            ;=> 0
(count-params :a)         ;=> 1
(count-params :a 1 :b :c) ;=> 4

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more))
  )

(my-*)           ;=> 1
(my-* 4 3)       ;=> 12
(my-* 1 2 3 4 5) ;=> 120

(defn pred-and
  ([] (fn [x]
        true
        ))
  ([pred] (fn [x]
            (pred x)
            ))
  ([pred1 pred2] (fn [x]
                   (and (pred1 x) (pred2 x))
                   ))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more))
  )

(filter (pred-and) [1 0 -2])                    ;=> (1 0 -2)
(filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> (1 7)
(filter (pred-and number? integer? pos? even?)
        [1 0 -2 :a 7 "a" 2])                    ;=> (0 2)

(defn my-map [f & s]

  )

(my-map inc [1 2 3 4])                  ;=> (2 3 4 5)
(my-map + [1 1 1] [1 1 1] [1 1 1])      ;=> (3 3 3)
(my-map vector [1 2 3] [1 2 3] [1 2 3]) ;=> ((1 1 1) (2 2 2) (3 3 3))
