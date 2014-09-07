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

(facts "Check my-*" :mine
       (fact "Check my-* for no params"
             (my-*) => 1)
       (fact "Check my-* for one param"
             (my-* 3) => 3)
       (fact "Check my-* for two params"
             (my-* 4 3) => 12)
       (fact "Check my-* for more than two params"
             (my-* 1 2 3 4 5) => 120))

(facts "Check pred-and" :mine
       (fact "Check non parameters"
             (filter (pred-and) [1 0 -2]) => '(1 0 -2))
       (fact "Check one predicate"
             (filter (pred-and pos?) [1 2 -4 0]) => '(1 2))
       (fact "Check two predicates"
             (filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) => '(1 7))
       (fact "Check four predicates"
             (filter (pred-and number? integer? pos? even?) [1 0 -2 :a 7 "a" 2])
             => '(2)))

(facts "Check my-map" :mine
       (fact "Check my-map for empty coll"
             (my-map inc []) => '())
       (fact "Check my-map for one seq"
             (my-map inc [1 2 3 4]) => '(2 3 4 5))
       (fact "Check my-map for two seqs"
             (my-map + [1 1 1] [2 2 2]) => '(3 3 3))
       (fact "Check my-map for three seqs"
             (my-map + [1 1 1] [1 1 1] [1 1 1]) => '(3 3 3))
       (fact "Check my-map for three seq for vector"
             (my-map vector [1 2 3] [1 2 3] [1 2 3]) => '((1 1 1) (2 2 2) (3 3 3))))
