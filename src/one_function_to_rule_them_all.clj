(ns one-function-to-rule-them-all)

;; Write the function (concat-elements a-seq) that takes a sequence of sequences
;; and concatenates them together with concat.
;; Don’t use apply to implement this function.
;; (concat-elements [])            ;=> ()
;; (concat-elements [[1 2]])       ;=> (1 2)
;; (concat-elements [[1 2] [3 4]]) ;=> (1 2 3 4)
(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

;; Write the function (str-cat a-seq) that takes a sequence of strings and catenates
;; them with one space character between each.
;; (str-cat ["I" "am" "Legend"])  ;=> "I am Legend"
;; (str-cat ["I" "am" "back"])    ;=> "I am back"
;; (str-cat ["more" " " "space"]) ;=> "more   space"
;; (str-cat [])                   ;=> ""

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce
      (fn [a b] (str a " " b))
      a-seq)))

;; Write the function (my-interpose x a-seq) that places x between every element of a-seq.
;; Keep in mind how conj works for vectors.
;; (my-interpose 0 [1 2 3])               ;=> (1 0 2 0 3)
;; (my-interpose "," ["I" "me" "myself"]) ;=> ("I" "," "me" "," "myself")
;; (my-interpose :a [1])                  ;=> (1)
;; (my-interpose :a [])                   ;=> ()

(defn my-interpose [x a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (butlast
      (reduce
        (fn [a b] (conj a b x))
        []
        a-seq))))

;; Write the function (my-count a-seq) that returns the length of a sequence.
;; Do not use count in your implementation.
;; (my-count [])      ;=> 0
;; (my-count [1 2 3]) ;=> 3
;; (my-count [1])     ;=> 1
(defn my-count [a-seq]
  (reduce
    (fn [count _] (inc count))
    0
    a-seq))

;; Write the function (my-reverse a-seq) that reverses a sequence.
;; (my-reverse [1 2 3]) ;=> (3 2 1)
;; (my-reverse [1 2])   ;=> (2 1)
;; (my-reverse [])      ;=> ()
(defn my-reverse [a-seq]
  (reduce
    (fn [a b] (conj a b))
    '()
    a-seq))

;; Write the function (min-max-element a-seq) that returns the maximal and minimal elements of a-seq
;; in a vertor like [min max].
;; (min-max-element [2 7 3 15 4]) ;=> [2 15]
;; (min-max-element [1 2 3 4])    ;=> [1 4]
;; (min-max-element [1])          ;=> [1 1]
;; (min-max-element [])
(defn min-max-element [a-seq]
  (reduce
    (fn [result element]
      [(min (first result) element)
       (max (last result)  element)])
    [Double/POSITIVE_INFINITY, Double/NEGATIVE_INFINITY]
    a-seq))

;; Write the function (insert sorted-seq n) that adds the number n into a sorted sequence of number.
;; The ordering of the sequence must be preserved.
;; You don’t need to use reduce for this, and you probably don’t want to.
;; (insert [] 2)      ;=> (2)
;; (insert [1 3 4] 2) ;=> (1 2 3 4)
;; (insert [1] 2)     ;=> (1 2)
(defn insert [sorted-seq n]
  (let [smaller (take-while (fn [element] (< element n)) sorted-seq)
        greater (drop (count smaller) sorted-seq)]
    (concat smaller [n] greater)))

;; Now implement (insertion-sort a-seq) using reduce and the function insert.
;; (insertion-sort [2 5 3 1]) ;=> (1 2 3 5)
;; (insertion-sort [1 2])     ;=> (1 2)
(defn insertion-sort [a-seq]
  (reduce
    insert
    []
    a-seq))

;; Write the fuction (parity a-seq) that picks into a set those elements of a-seq that occur odd number of time.
;; (parity [:a :b :c])    ;=> #{:a :b :c}
;; (parity [:a :a :b :b]) ;=> #{}
;; (parity [1 2 3 1])     ;=> #{2 3}
(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

;; Write the function minus that takes one or two parameters.
;; If given a one parameter x, it returns −x.
;; If given to parameters x and y, it returns x−y.
;; (minus 2)   ;=> -2
;; (minus 4 3) ;=> 1
(defn minus
  ([x] (- x))
  ([x y] (- x y)))


;; Write the function count-params that accepts any number of parameters and returns how many it was called with.
;; You need only a one definition for this.
;; (count-params)            ;=> 0
;; (count-params :a)         ;=> 1
;; (count-params :a 1 :b :c) ;=> 4
(defn count-params [& params]
  (count params))

;; Write the function my-* that takes any number of parameters.
;; If no parameters are given, return 1
;; If one parameter x is given, return x.
;; If two parameters x and y are given, return xy.
;; If more than two parameters x, y, …… are given, return their product
;; You are free to use *, but not apply.
;; (my-*)           ;=> 1
;; (my-* 4 3)       ;=> 12
;; (my-* 1 2 3 4 5) ;=> 120
(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

;; Remember the function pred-and that you implemented in Predicates? Write a new definition for it that works for any
;; amount of parameters.
;; If no parameters are given, return a predicate that always returns true.
;; If only one predicate p is given, return p.
;; If two predicates are given, return a predicate that returns true if both of them return true and false otherwise.
;; If more than two predicates are given, return a predicate that returns true only if all of the predicates return
;; true and false otherwise.
;; (filter (pred-and) [1 0 -2])                    ;=> (1 0 -2)
;; (filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> (1 7)
;; (filter (pred-and number? integer? pos? even?)
;;         [1 0 -2 :a 7 "a" 2])                    ;=> (0 2)
(defn pred-and
  ([]
   (fn [x] true))
  ([pred1] pred1)
  ([pred1 pred2]
   (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more]
   (reduce pred-and (pred-and pred1 pred2) more)))


;; Write the function my-map that works just like standard map. It takes one or more sequences and a
;; function f that takes as many parameters as there are sequences.
;; (my-map inc [1 2 3 4])                  ;=> (2 3 4 5)
;; (my-map + [1 1 1] [1 1 1] [1 1 1])      ;=> (3 3 3)
;; (my-map vector [1 2 3] [1 2 3] [1 2 3]) ;=> ((1 1 1) (2 2 2) (3 3 3))

(defn foo [f seq-of-seqs]
  (reduce
    (fn[res a-seq] (conj res (f a-seq)))
    []
    seq-of-seqs))

(defn firsts [a-seq]
  (foo first a-seq))

(defn rests [a-seq]
  (foo rest a-seq))

(defn rearrange [a-seq]
  (cond
    (= 0 (count (first a-seq))) []
    (= 1 (count (first a-seq))) [(firsts a-seq)]
    :else (cons
            (firsts a-seq)
            (rearrange (rests a-seq)))))

(defn my-map [f & seq-of-seqs]
  (reduce
    (fn [result a-seq]
      (do
        (println result a-seq)
        (conj result (apply f a-seq))))
    []
    (rearrange seq-of-seqs)))
