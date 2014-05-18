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
(ctest/is (= (concat-elements [])            ()))
;; these fail but it is working??
(ctest/is (= (concat-elements [[1 2]])       (1 2)))
(ctest/is (= (concat-elements [[1 2] [3 4]]) (1 2 3 4)))



(defn str-cat [a-seq]
  :-)

(defn my-interpose [x a-seq]
  [:-])

(defn my-count [a-seq]
  :-)

(defn my-reverse [a-seq]
  [:-])

(defn min-max-element [a-seq]
  [:-])

(defn insert [sorted-seq n]
  [:-])

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  [:-])

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
