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
