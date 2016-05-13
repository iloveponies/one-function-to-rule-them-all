(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (if (empty? a-seq)
    '()
    (concat (reduce into [] a-seq))))

;; (concat-elements [])            ;=> ()
;; (concat-elements [[1 2]])       ;=> (1 2)
;; (concat-elements [[1 2] [3 4]]) ;=> (1 2 3 4)

(defn str-cat [a-seq]
  (if (empty? a-seq)
    '""
    (reduce str (interpose " " a-seq))
    ))

;; (str-cat ["I" "am" "Legend"])  ;=> "I am Legend"
;; (str-cat ["I" "am" "back"])    ;=> "I am back"
;; (str-cat ["more" " " "space"]) ;=> "more   space"
;; (str-cat [])                   ;=> ""

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce interpose (conj [x] a-seq))))

;; (my-interpose 0 [1 2 3])               ;=> (1 0 2 0 3)
;; (my-interpose "," ["I" "me" "myself"]) ;=> ("I" "," "me" "," "myself")
;; (my-interpose :a [1])                  ;=> (1)
;; (my-interpose :a [])                   ;=> ()

(defn my-count [a-seq]
  (let [theount 0
        counter (fn [thecount a-seq] (inc thecount))]
  (if (empty? a-seq)
    0
    (reduce counter 0 a-seq))))

;; (my-count [1 4])
;; (my-count [])      ;=> 0
;; (my-count [1 2 3]) ;=> 3
;; (my-count [6])     ;=> 1

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

;; (my-reverse [1 2 3]) ;=> (3 2 1)
;; (my-reverse [1 2])   ;=> (2 1)
;; (my-reverse [])      ;=> ()

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  [:-])

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]

  (let [])
)

;; (parity [:a :b :c])    ;=> #{:a :b :c}
;; (parity [:a :a :b :b]) ;=> #{}
;; (parity [1 2 3 1])     ;=> #{2 3}


(defn minus
  ([x] (- x (* x 2)))
  ([x y] (- x y)))

;; (minus 2)   ;=> -2
;; (minus 4 3) ;=> 1

(defn count-params [& more]
  (count more))

;; (count-params)            ;=> 0
;; (count-params :a)         ;=> 1
;; (count-params :a 1 :b :c) ;=> 4

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

;; (my-*)           ;=> 1
;; (my-* 4 3)       ;=> 12
;; (my-* 1 2 3 4 5) ;=> 120

;; (defn pred-and [pred1 pred2]
;;   (fn [x] (if (and
;;                   (pred1 x)
;;                   (pred2 x))
;;               true
;;               false)))

(defn pred-and
    ([] (fn [x] x))
    ([p] (fn [x] (p x)))
    ([p q] (fn [x] (boolean (and (p x) (q x)))))
    ([p q & more] (fn [x] (boolean (and
                                     (p x)
                                     (q x)
                                     (every-pred more x))))))


;; (filter (pred-and) [1 0 -2])                    ;=> (1 0 -2)
;; (filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> (1 7)
;; (filter (pred-and number? integer? pos? even?) [1 0 -2 :a 7 "a" 2])                    ;=> (0 2)


(defn my-map [f a-seq]
  [:-])
