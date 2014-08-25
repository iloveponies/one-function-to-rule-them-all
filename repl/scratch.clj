(use 'one-function-to-rule-them-all :reload)

(my-count [])      ;=> 0
(my-count [1 2 3]) ;=>
(reduce (fn ([a b] str "(f:" a ") s: (" b ")" ) [1 2 3])

(my-count [1])     ;=> 1

(reduce (fn [a b] (inc a)) 0 [])

