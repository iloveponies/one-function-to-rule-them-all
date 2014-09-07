(ns one-function-to-rule-them-all-test
  (:use iloveponies.tests.one-function-to-rule-them-all
        one-function-to-rule-them-all
        midje.sweet))


(fact "Check insert function" :mine
      (insert [] 2) => '(2)
      (insert [1 3 4] 2) => '(1 2 3 4)
      (insert [2] 1) => '(1 2)
      (insert [1] 2) => '(1 2))

(fact "Check insertion-sort function" :mine
      (insertion-sort []) => '()
      (insertion-sort [2 5 3 1]) => '(1 2 3 5)
      (insertion-sort [1 2]) => '(1 2))

(fact "Check parity." :mine
      (parity [:a :b :c]) => #{:a :b :c}
      (parity [:a :a :b :b]) => #{}
      (parity [1 2 3 1]) => #{2 3}
      (parity []) => #{})

(facts "Check minus function." :mine
       (fact "Check minus with one param"
             (minus 2) => -2)
       (fact "Check minus with two params"
             (minus 4 3) => 1
             (minus 3 5) => -2))

(facts "Check count-params" :mine
       (fact "Check count-params with no params"
             (count-params) => 0)
       (fact "Check count-params with one param"
             (count-params :a) => 1)
       (fact "Check count-params with four params"
             (count-params :a 1 :b :c) => 4))
