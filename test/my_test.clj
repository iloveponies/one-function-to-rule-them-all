(ns my-test
  (:use midje.sweet
	one-function-to-rule-them-all))

(facts "my-interpose" {:exercise 3
                       :points 1}
  (my-interpose 0 [1 2 3])
  => [1 0 2 0 3]
  (my-interpose "," ["I" "me" "myself"])
  => (just '("I" "," "me" "," "myself"))
  (my-interpose :a [[1] [2] [3]])
  => (just '([1] :a [2] :a [3]))
  (my-interpose :a [1])
  => [1]
  (my-interpose :a [])
  => [])

