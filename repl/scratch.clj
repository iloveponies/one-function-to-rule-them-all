(use 'one-function-to-rule-them-all :reload)

(my-count [])      ;=> 0
(my-count [1 2 3]) ;=>
(my-count [1 2 3 8])     ;=> 1

(my-reverse [1 2 3]) ;=> (3 2 1)
(my-reverse [1 2])   ;=> (2 1)
(my-reverse [])      ;=> ()

(min-max-element [2 7 3 15 4]) ;=> [2 15]
(min-max-element [1 2 3 4])    ;=> [1 4]
(min-max-element [1])          ;=> [1 1]
(min-max-element [])          ;=> [1 1]

(insert [] 2)      ;=> (2)
(insert [1 3 4] 2) ;=> (1 2 3 4)
(insert [1] 2)     ;=> (1 2)

(insertion-sort [2 5 3 1]) ;=> (1 2 3 5)
(insertion-sort [1 2])     ;=> (1 2)

(parity [:a :b :c])    ;=> #{:a :b :c}
(parity [:a :a :b :b]) ;=> #{}
(parity [1 2 3 1])     ;=> #{2 3}

(minus 2)   ;=> -2
(minus 4 3) ;=> 1

(count-params)            ;=> 0
(count-params :a)         ;=> 1
(count-params :a 1 :b :c) ;=> 4

(my-*)           ;=> 1
(my-* 4 3)       ;=> 12
(my-* 1 2 3 4 5) ;=> 120

(filter (pred-and) [1 0 -2])                    ;=> (1 0 -2)
(filter (pred-and even?) [1 0 -2])                    ;=> (1 0 -2)
(filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> (1 7)
(filter (pred-and number? integer? pos? even?)
        [1 0 -2 :a 7 "a" 2])                    ;=> (0 2)


(firsts '((1 2 3)[4 5 6]#{7 8 9}))
(nexts '((1 2 3)(4 5 6)#{7 8 9}))
(any-empty? [[][2][4]])


(map str [1 2 9 3 4][4 7 5 8 6][7 8 8 9 3])
(my-map str [1 2 9 3 4][4 7 5 8 6][7 8 8 9 3])


;;
