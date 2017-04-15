(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

;(concat-elements [])            ;=> ()
;(concat-elements [[1 2]])       ;=> (1 2)
;(concat-elements [[1 2] [3 4]]) ;=> (1 2 3 4)

(defn str-cat [a-seq]
  (let [asdasd (interpose " " a-seq)]
  (reduce str asdasd)))

;(str-cat ["I" "am" "Legend"])  ;=> "I am Legend"
;(str-cat ["I" "am" "back"])    ;=> "I am back"
;(str-cat ["more" " " "space"]) ;=> "more   space"
;(str-cat [])                   ;=> ""

(defn my-interpose [x a-seq]
  [:-])

(defn my-count [a-seq]
  (let [c (fn [count x]
            (inc count))]
  (reduce c 0 a-seq)))

;(my-count [])      ;=> 0
;(my-count [1 2 3]) ;=> 3
;(my-count [1])     ;=> 1

(defn my-reverse [a-seq]
 )

;(my-reverse [1 2 3]) ;=> (3 2 1)
;(my-reverse [1 2])   ;=> (2 1)
;(my-reverse [])      ;=> ()

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  [:-])

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  [:-])

(defn minus [x]
  :-)

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
