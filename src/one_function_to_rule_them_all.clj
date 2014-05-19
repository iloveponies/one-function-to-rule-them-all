(ns one-function-to-rule-them-all
  (:require [clojure.test :as ctest]))


;; Exercise 1
;; Write the function (concat-elements a-seq) that takes a sequence of sequences and concatenates them together with concat.
;; Don’t use apply to implement this function.
;;
;; sig: seq of seqs -> seq
;; purpose flatten a nested structure
;; stab:
;; (defn concat-elements [a-seq]
;;   :-)
;;
(defn concat-elements [a-seq]
  (reduce concat a-seq))
;;
;; careful. these need ' before ()
(ctest/is (= (concat-elements [])            '()))
(ctest/is (= (concat-elements [[1 2]])       '(1 2)))
(ctest/is (= (concat-elements [[1 2] [3 4]]) '(1 2 3 4)))


;; Exercise 2
;; Write the function (str-cat a-seq) that takes a sequence of strings and catenates them with one space character between each.
;; You probably want to handle the special case of empty parameter outside reduce.
;;
;; sig: seq of strings -> string
;; purpose: concatenate strings into one string with a single space in between
;; stub:
;; (defn str-cat [a-seq]
;;   ":-")
;;
(defn str-cat [a-seq]
  (reduce #(str %1 " " %2) a-seq))
;;
(ctest/is (= (str-cat ["I" "am" "Legend"])  "I am Legend"))
(ctest/is (= (str-cat ["I" "am" "back"])    "I am back"))
(ctest/is (= (str-cat ["more" " " "space"]) "more   space"))
(ctest/is (= (str-cat [])                   ""))


;; Exercise 3
;; Write the function (my-interpose x a-seq) that places x between every element of a-seq.
;; Keep in mind how conj works for vectors.
;;
;; signature: val, seq -> seq
;; purpose: place val in between elements of seq
;; stub
;; (defn my-interpose [x a-seq]
;;   [:-])
;;
(defn my-interpose [x a-seq]
  (rest (reduce #(concat %1 [x %2]) [] a-seq)))
;;
(ctest/is (= (my-interpose 0 [1 2 3])               '(1 0 2 0 3)))
(ctest/is (= (my-interpose "," ["I" "me" "myself"]) '("I" "," "me" "," "myself")))
(ctest/is (= (my-interpose :a [1])                  '(1)))
(ctest/is (= (my-interpose :a [])                   '()))



;; Exercise 4
;; Write the function (my-count a-seq) that returns the length of a sequence.
;; Do not use count in your implementation.
;;
;; signature: seq -> number
;; purpose: count elements of seq
;; stub:
;; (defn my-count [a-seq]
;;   -1)
;;
(defn my-count [a-seq]
  ;; just keep adding to a, b (new element from seq) is ignored
  (reduce (fn [a b] (inc a)) 0 a-seq))
;;
(ctest/is (= (my-count [])      0))
(ctest/is (= (my-count [1 2 3]) 3))
(ctest/is (= (my-count [1])     1))



;; Exercise 5
;; Write the function (my-reverse a-seq) that reverses a sequence.
;;
;; signature: seq -> seq
;; purpose: reverse sequence
;; stub
;; (defn my-reverse [a-seq]
;;   [:-])
;;
(defn my-reverse [a-seq]
  (reduce #(conj %1 %2) '() a-seq))
;;
(ctest/is (= (my-reverse [1 2 3])  '(3 2 1)))
(ctest/is (= (my-reverse [1 2])    '(2 1)))
(ctest/is (= (my-reverse [])       '()))


;; Exercise 6
;; Write the function (min-max-element a-seq) that returns the maximal and minimal elements of a-seq in a vertor like [min max].
;;
;; signature: seq -> seq
;; purpose: return [min max] as a seq
;; stub
;; (defn min-max-element [a-seq]
;;   [nil nil])
;;
(defn min-max-element [a-seq]
  ;; assign very first element
  (let [fst (first a-seq)]
    ;; loop over 
    (loop [sq  a-seq
           min fst
           max fst]
      
      (cond
       (empty? sq)        [min max]
       (> (first sq) max) (recur (rest sq) min        (first sq))
       (< (first sq) min) (recur (rest sq) (first sq) max)
       :else              (recur (rest sq) min        max)))))
;;
(ctest/is (= (min-max-element [2 7 3 15 4])  [2 15]))
(ctest/is (= (min-max-element [1 2 3 4])     [1 4]))
(ctest/is (= (min-max-element [1])           [1 1]))


;; Exercise 7
;; Write the function (insert sorted-seq n) that adds the number n into a sorted sequence of number. The ordering of the sequence must be preserved.
;; You don’t need to use reduce for this, and you probably don’t want to.
;;
;; signature: seq, number -> seq
;; purpose: insert number to a sequence
;; stub
;; (defn insert [sorted-seq n]
;;   [nil nil nil])
;;
(defn insert [sorted-seq n]
  (loop [passed    []
         remaining sorted-seq]
    (cond
     ;; Exiting at the end of seq
     (empty? remaining)                 (conj passed n)
     ;; Exisiting in the middle of seq
     (< n (first remaining))            (concat (conj passed n) remaining)
     ;; Otherwise recur
     :else                              (recur (conj passed (first remaining)) (rest remaining)))))
;;
(ctest/is (= (insert [] 2)       '(2)))
(ctest/is (= (insert [1 3 4] 2)  '(1 2 3 4)))
(ctest/is (= (insert [1] 2)      '(1 2)))

;; Now implement (insertion-sort a-seq) using reduce and the function insert.
;;
;; sig: seq -> seq
;; Sort by insertion
;; stub
;; (defn insertion-sort [a-seq]
;;   [nil nil nil])
;;
(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))
;;
(ctest/is (= (insertion-sort [2 5 3 1])  '(1 2 3 5)))
(ctest/is (= (insertion-sort [1 2])      '(1 2)))


;; Exercise 8
;; Write the fuction (parity a-seq) that picks into a set those elements of a-seq that occur odd number of time.
;;
;; sig: seq -> set
;; Keep ones appearing odd number of times
;; stub:
;; (defn parity [a-seq]
;;   #{nil})
;;
(defn parity [a-seq]
  (let [in-out (fn [aset aval]
                 (if (contains? aset aval)
                   (disj aset aval)
                   (conj aset aval)))]
    
    (reduce in-out #{} a-seq)))
;;
(ctest/is (= (parity [:a :b :c])     #{:a :b :c}))
(ctest/is (= (parity [:a :a :b :b])  #{}))
(ctest/is (= (parity [1 2 3 1])      #{2 3}))


;; Exercise 9
;; Write the function minus that takes one or two parameters.
;; If given a one parameter x, it returns −x.
;; If given to parameters x and y, it returns x−y.
;;
;; sig: number, (number) -> number
;; negate or subtract depending on the number of args
;; 
(defn minus
  ([x]   (- x))
  ([x y] (- x y)))
;;
(ctest/is (= (minus 2)   -2))
(ctest/is (= (minus 4 3) 1))


;; Exercise 10
;; Write the function count-params that accepts any number of parameters and returns how many it was called with. You need only a one definition for this.
;;
;; sig: any args -> number
;; purpose: count args
;; stub
(defn count-params [x]
  -1)
;;
(defn count-params [& args]
  (count args))
;;
(ctest/is (= (count-params)            0))
(ctest/is (= (count-params :a)         1))
(ctest/is (= (count-params :a 1 :b :c) 4))


;; Exercise 11
;; Write the function my-* that takes any number of parameters.
;; If no parameters are given, return 1
;; If one parameter x is given, return x.
;; If two parameters x and y are given, return xy.
;; If more than two parameters x, y, … are given, return their product x⋅y⋯.
;; You are free to use *, but not apply.
;;
;; sig numbers -> number
;; purpose
(defn my-* [& args]
  (reduce * 1 args))
;;
(ctest/is (= (my-*)           1))
(ctest/is (= (my-* 4 3)       12))
(ctest/is (= (my-* 1 2 3 4 5) 120))


;; Exercise 12
;; Remember the function pred-and that you implemented in Predicates? Write a new definition for it that works for any amount of parameters.
;; If no parameters are given, return a predicate that always returns true.
;; If only one predicate p is given, return p.
;; If two predicates are given, return a predicate that returns true if both of them return true and false otherwise.
;; If more than two predicates are given, return a predicate that returns true only if all of the predicates return true and false otherwise.
;;
;; sig: pred1, pred2 -> fun
;; purpose: create a new predicate (and pred1 pred2)
;; 
(defn pred-and [x]
  (fn [x] :-))
;;
(ctest/is (= (filter (pred-and) [1 0 -2])                    '(1 0 -2)))
(ctest/is (= (filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) '(1 7)))
(ctest/is (filter (pred-and number? integer? pos? even?) [1 0 -2 :a 7 "a" 2]) '(0 2))



;; Exercise 13
;; 3 points
;; Write the function my-map that works just like standard map. It takes one or more sequences and a function f that takes as many parameters as there are sequences.

(defn my-map [f a-seq]
  [:-])
;;
(ctest/is (= (my-map inc [1 2 3 4])                  (2 3 4 5)))
(ctest/is (= (my-map + [1 1 1] [1 1 1] [1 1 1])      (3 3 3)))
(ctest/is (= (my-map vector [1 2 3] [1 2 3] [1 2 3]) ((1 1 1) (2 2 2) (3 3 3))))
