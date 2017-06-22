(ns one-function-to-rule-them-all)


;; Ex 1
;; Write the function (concat-elements a-seq) that takes a sequence of sequences and concatenates them together with concat.
;; Don’t use apply to implement this function.

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))


;; Ex 2
;; Write the function (str-cat a-seq) that takes a sequence of strings and catenates them with one space character between each.

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))


;; Ex 3
;; Write the function (my-interpose x a-seq) that places x between every element of a-seq.
;; Keep in mind how conj works for vectors.

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce #(concat %1 (list x %2)) (list (first a-seq)) (rest a-seq))))


;; Ex 4
;; Write the function (my-count a-seq) that returns the length of a sequence.
;; Do not use count in your implementation.

(defn my-count [a-seq]
  (reduce (fn [x _] (+ x 1)) 0 a-seq))


;; Ex 5
;; Write the function (my-reverse a-seq) that reverses a sequence.

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))


;; Ex 6
;; Write the function (min-max-element a-seq) that returns the maximal and minimal elements of a-seq in a vertor like [min max].

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    [nil nil]
    (if (empty? (rest a-seq)) ; must be a single-element list
      [(first a-seq) (first a-seq)]
      (let [largest #(if (> %1 %2) %1 %2)
            smallest #(if (< %1 %2) %1 %2)]
      (reduce (fn [minmax elem]
                [(smallest (get minmax 0) elem) (largest (get minmax 1) elem)])
              (apply vector (take 2 a-seq)) a-seq)))))


;; Ex 7
;; Write the function (insert sorted-seq n) that adds the number n into a sorted sequence of number. The ordering of the sequence must be preserved.
;; You don’t need to use reduce for this, and you probably don’t want to.

(defn insert [sorted-seq n]
  (let [[s1 s2] (split-with #(> n %1) sorted-seq)]
    (concat s1 (cons n s2))))


;; Ex 7 - contd.
;; Now implement (insertion-sort a-seq) using reduce and the function insert.

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))


;; Ex 8
;; Write the fuction (parity a-seq) that picks into a set those elements of a-seq that occur odd number of time.

(defn parity [a-seq]
  (reduce #((if (contains? %1 %2) disj conj) %1 %2) #{} a-seq))


;; Ex 9
;; Write the function minus that takes one or two parameters.
;; If given a one parameter xx, it returns −x−x.
;; If given to parameters xx and yy, it returns x−yx−y.

(defn minus
  ([x] (- x))
  ([x y] (- x y)))


;; Ex 10
;; Write the function count-params that accepts any number of parameters and returns how many it was called with. You need only a one definition for this.

(defn count-params
 ([& more] (count more)))



;; Ex 11
;; Write the function my-* that takes any number of parameters.
;; If no parameters are given, return 1
;; If one parameter xx is given, return xx.
;; If two parameters xx and yy are given, return xyxy.
;; If more than two parameters xx, yy, …… are given, return their product x⋅y⋯x⋅y⋯.
;; You are free to use *, but not apply.

(defn my-*
 ([] 1)
 ([x] x)
 ([x y] (* x y))
 ([x y & more] (reduce my-* (my-* x y) more)))


;; Ex 12
;; Remember the function pred-and that you implemented in Predicates? Write a new definition for it that works for any amount of parameters.
;; If no parameters are given, return a predicate that always returns true.
;; If only one predicate p is given, return p.
;; If two predicates are given, return a predicate that returns true if both of them return true and false otherwise.
;; If more than two predicates are given, return a predicate that returns true only if all of the predicates return true and false otherwise.

(defn pred-and
  ([] (fn [arg] true))
  ([pred?] (fn [arg] (pred? arg)))
  ([pred1? pred2?] (fn [arg] (and (pred1? arg) (pred2? arg))))
  ([pred1? pred2? & more] (reduce pred-and (pred-and pred1? pred2?) more)))



;; E N C O R E   S E C T I O N  ---  B O N U S   M A R K S   F O R   T H E S E   ! ! !



;; Exercise 13
;; 3 points
;; Write the function my-map that works just like standard map. It takes one or more sequences and a function f that takes as many parameters as there are sequence

(defn my-map
  ([f a-seq ]
    (reduce #(concat %1 (list (f %2))) '() a-seq))
  ([f a-seq & more]
   (reverse
     (loop [accum '()
            f f
            a-seq (cons a-seq more)]
       (if (some empty? a-seq)
         accum
         (recur (cons (apply f (my-map first a-seq)) accum) f (map rest a-seq)))))))

