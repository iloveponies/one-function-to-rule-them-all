(ns one-function-to-rule-them-all-test
  (:use midje.sweet
        one-function-to-rule-them-all))

(facts "1 concat-elements"
       (concat-elements [])            => (just '())
       (concat-elements [[1 2]])       => (just '(1 2))
       (concat-elements [[1 2] [3 4]]) => (just '(1 2 3 4))
       (concat-elements [[1 2 2] [2 3]]) => (just '(1 2 2 2 3)))

(facts "2 str-cat"
       (str-cat ["I" "am" "Legend"])  => "I am Legend"
       (str-cat ["I" "am" "back"])    => "I am back"
       (str-cat ["more" " " "space"]) => "more   space"
       (str-cat [])                   => "")

(facts "3 my-interpose"
  (my-interpose 0 [1 2 3])
  => [1 0 2 0 3]
  (my-interpose "," ["I" "me" "myself"])
  => (just '("I" "," "me" "," "myself"))
  (my-interpose :a [1])
  => [1]
  (my-interpose :a [])
  => [])

(facts "4 my-count"
  (my-count [])      => 0
  (my-count [1 2 3]) => 3
  (my-count [1])     => 1)

(facts "5 my-reverse"
  (my-reverse [1 2 3])    => (just '(3 2 1))
  (my-reverse [1 2 2 3 3] => (just '(3 3 2 2 1)))
  (my-reverse [1 2])      => (just '(2 1))
  (my-reverse [])         => (just '()))

(facts "6 min-max-element"
       (min-max-element [2 7 3 15 4]) => [2 15]
       (min-max-element [1 2 3 4])    => [1 4]
       (min-max-element [1])          => [1 1])

(facts "7 insert"
       (insert [] 2)      => (just '(2))
       (insert [1 3 4] 2) => (just '(1 2 3 4))
       (insert [3 5 7] 2) => (just '(2 3 5 7))
       (insert [1] 2)     => (just '(1 2)))

(facts "7 insertion-sort"
  (insertion-sort [2 5 3 1])             => (just '(1 2 3 5))
  (insertion-sort (shuffle (range 100))) => (range 100)
  (insertion-sort [1 2])                 => (just '(1 2)))

(facts "8 parity"
       (parity [:a :b :c])    => #{:a :b :c}
       (parity [:a :a :b :b]) => #{}
       (parity [1 2 3 1])     => #{2 3})

(facts "9 minus"
       (minus 2)   => -2
       (minus 4 3) => 1)

(facts "10 count-params"
       (count-params)            => 0
       (count-params :a)         => 1
       (count-params :a 1 :b :c) => 4)

(facts "11 my-*"
       (my-*) => 1
       (my-* 4 3) => 12
       (my-* 1 2 3 4 5) => 120)

(facts "12 pred-and"
       (filter (pred-and) [1 0 -2])                    => [1 0 -2]
       (filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) => [1 7]
       (filter (pred-and number? integer? pos? even?)
               [1 0 -2 :a 7 "a" 2])                    => [2])

(facts "13 my-map"
       (my-map inc [1 2 3 4])                  => [2 3 4 5]
       (my-map + [1 1 1] [1 1 1] [1 1 1])      => [3 3 3]
       (my-map vector [1 2 3] [1 2 3] [1 2 3]) => [[1 1 1] [2 2 2] [3 3 3]])
