(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(concat-elements [])            ;=> ()
(concat-elements [[1 2]])       ;=> (1 2)
(concat-elements [[1 2] [3 4]]) ;=> (1 2 3 4)

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(str-cat ["I" "am" "Legend"])  ;=> "I am Legend"
(str-cat ["I" "am" "back"])    ;=> "I am back"
(str-cat ["more" " " "space"]) ;=> "more   space"
(str-cat [])                   ;=> ""


(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (reduce
      (fn [acc curr] (conj acc x curr))
      [(first a-seq)] (rest a-seq))))

(my-interpose 0 [1 2 3])               ;=> (1 0 2 0 3)
(my-interpose "," ["I" "me" "myself"]) ;=> ("I" "," "me" "," "myself")

(defn my-count [a-seq]
  (reduce (fn [acc sek] (inc acc)) 0 a-seq))

(my-count [1 2 3])

(defn my-reverse [a-seq]
  (let [combine (fn [acc curr] (cons curr acc))]
    (reduce combine () a-seq)))

(my-reverse [1 2 3]) ;=> (3 2 1)
(my-reverse [1 2])   ;=> (2 1)
(my-reverse [])      ;=> ()

(defn min-max-element [a-seq]
  (vector (reduce min a-seq) (reduce max a-seq)))

(min-max-element [2 7 3 15 4]) ;=> [2 15]
(min-max-element [1 2 3 4])    ;=> [1 4]
(min-max-element [1])          ;=> [1 1]


(defn insert [sorted-seq n]
  (loop [s sorted-seq a []]
    (cond
      (empty? s) (conj a n)
      (< (first s) n) (recur (rest s) (conj a (first s)))
    :else            (apply conj a n s))))

(defn insertion-sort [a-seq]
  (let [srt (fn [acc new] (insert acc new))]
    (reduce srt [] a-seq)))

(insertion-sort [8 1 6 4 2 3])

(defn toggle [a-set n]
  (if (contains? a-set n)
    (disj a-set n)
    (conj a-set n)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(parity [1 2 31 3 2])

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(minus 2 1)

(defn count-params
  ([] 0)
  ([& more]
   (if (= 1 (count more))
     1
     (reduce (fn [acc x] (inc acc)) more))))

(count-params :a)
(count-params)

(defn my-*
  ([] 1)
  ([& more]
     (reduce (fn [acc y] (* acc y)) 1 (seq more))))

(my-*)

(defn pred-and
  ([] (fn [x] true))
  ([pred1?] pred1?)
  ([pred1? pred2?] (fn [x] (and (pred1? x) (pred2? x))))
  ([pred1? pred2? & more] (reduce pred-and (pred-and pred1? pred2?) more)))

(pred-and (filter (pred-and) [1 0 -2]))
          ;=> (1 0 -2)


(defn my-map [f a-seq]
  [:-])
