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


Exercise 7
Write the function (insert sorted-seq n) that adds the number n into a sorted sequence of number. The ordering of the sequence must be preserved.
You don’t need to use reduce for this, and you probably don’t want to.
(defn insert [sorted-seq n]
  [:-])
;;
(ctest/is (= (insert [] 2)       '(2)))
(ctest/is (= (insert [1 3 4] 2)  '(1 2 3 4)))
(ctest/is (= (insert [1] 2)      '(1 2)))

Now implement (insertion-sort a-seq) using reduce and the function insert.
(defn insertion-sort [a-seq]
  [:-])
(ctest/is (= (insertion-sort [2 5 3 1])  '(1 2 3 5)))
(ctest/is (= (insertion-sort [1 2])      '(1 2)))


Exercise 8
Write the fuction (parity a-seq) that picks into a set those elements of a-seq that occur odd number of time.
(defn parity [a-seq]
  [:-])
(ctest/is (= (parity [:a :b :c])     #{:a :b :c}))
(ctest/is (= (parity [:a :a :b :b])  #{}))
(ctest/is (= (parity [1 2 3 1])      #{2 3}))


(defn minus [x]
  :-)

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
