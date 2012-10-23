(ns one-function-to-rule-them-all-test
  (:use midje.sweet
        one-function-to-rule-them-all))

(facts "concat-elements"
       (concat-elements [])            => (just '())
       (concat-elements [[1 2]])       => (just '(1 2))
       (concat-elements [[1 2] [3 4]]) => (just '(1 2 3 4)))

(facts "str-cat"
       (str-cat ["I" "am" "Legend"]) => "I am Legend"
       (str-cat ["I" "am" "back"])   => "I am back"
       (str-cat ["more" " " "space"]) => "more   space")

(facts "my-interpose"
       (my-interpose 0 [1 2 3])               => (just '(1 0 2 0 3))
       (my-interpose "," ["I" "me" "myself"])
       => (just '("I" "," "me" "," "myself"))
       (my-interpose :a [1])                  => (just '(1))
       (my-interpose :a [])                   => (just '()))

(facts "my-count"
       (my-count [])      => 0
       (my-count [1 2 3]) => 3
       (my-count [1])     => 1)

(facts "my-reverse"
       (my-reverse [1 2 3]) => (just '(3 2 1))
       (my-reverse [1 2])   => (just '(2 1))
       (my-reverse [])      => (just '()))

(facts "min-max-element"
       (min-max-element [2 7 3 15 4]) => (just '(2 15))
       (min-max-element [1 2 3 4])    => (just '(1 4))
       (min-max-element [1])          => (just '(1 1)))

(facts "insert"
       (insert [] 2)      => (just '(2))
       (insert [1 3 4] 2) => (just '(1 2 3 4))
       (insert [1] 2)     => (just '(1 2)))

(facts "insertion-sort"
       (insertion-sort [2 5 3 1]) => (just '(1 2 3 5))
       (insertion-sort [1 2])     => (just '(1 2)))

(facts "parity"
       (parity [:a :b :c])    => #{:a :b :c}
       (parity [:a :a :b :b]) => #{}
       (parity [1 2 3 1])     => #{2 3})