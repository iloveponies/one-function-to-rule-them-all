(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq ))

;(concat-elements [])            ;=> ()
;(concat-elements [[1 2]])       ;=> (1 2)
;(concat-elements [[1 2] [3 4]]) ;=> (1 2 3 4)

(defn str-cat [a-seq]
  (if(empty? a-seq)(str "")
    (reduce str(interpose " " a-seq))
    ))

;(str-cat ["I" "am" "Legend"])  ;=> "I am Legend"
;(str-cat ["I" "am" "back"])    ;=> "I am back"
;(str-cat ["more" " " "space"]) ;=> "more   space"
;(str-cat [])                   ;=> ""




(defn my-interpose [x a-seq]
   (reduce (fn [z y] (if (empty? z)
                       (conj z y) (conj z x y)))
           [] a-seq ) )


;(my-interpose 0 [1 2 3])               ;=> (1 0 2 0 3)
;(my-interpose "," ["I" "me" "myself"]) ;=> ("I" "," "me" "," "myself")
;(my-interpose :a [1])                  ;=> (1)
;(my-interpose :a [])                   ;=> ()


(defn my-count [a-seq]
  (if(empty? a-seq) 0
  (reduce (fn [x y](inc x)) a-seq)))

;(my-count [])      ;=> 0
;(my-count [1 2 3]) ;=> 3
;(my-count [1])     ;=> 1

(defn my-reverse [a-seq]
  [:-])

(defn min-max-element [a-seq]
  [:-])

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
