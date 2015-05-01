(ns one-function-to-rule-them-all)


; EXERCISE 1
(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

;(concat-elements [])            ;=> ()
;(concat-elements [[1 2]])       ;=> (1 2)
;(concat-elements [[1 2] [3 4]]) ;=> (1 2 3 4)


; EXERCISE 2
(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y))
            a-seq)))

;(str-cat ["I" "am" "Legend"])  ;=> "I am Legend"
;(str-cat ["I" "am" "back"])    ;=> "I am back"
;(str-cat ["more" " " "space"]) ;=> "more   space"
;(str-cat [])                   ;=> ""


; EXERCISE 3
(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (let [helper (fn [acc
                      elem]
                   (concat (concat acc [x]) [elem]))]
     (reduce helper [(first a-seq)] (rest a-seq)))))

;(my-interpose 0 [1 2 3])               ;=> (1 0 2 0 3)
;(my-interpose "," ["I" "me" "myself"]) ;=> ("I" "," "me" "," "myself")
;(my-interpose :a [1])                  ;=> (1)
;(my-interpose :a [])                   ;=> ()


; EXERCISE 4
(defn my-count [a-seq]
  (let [counter (fn [n elem]
                  (inc n))]
    (reduce counter 0 a-seq)))

;(my-count [])      ;=> 0
;(my-count [1 2 3]) ;=> 3
;(my-count [1])     ;=> 1


; EXERCISE 5
(defn my-reverse [a-seq]
  (let [reverser (fn [a b]
                   (concat [b] a))]
    (reduce reverser [] a-seq)))

;(my-reverse [1 2 3]) ;=> (3 2 1)
;(my-reverse [1 2])   ;=> (2 1)
;(my-reverse [])      ;=> ()


; EXERCISE 6
(defn min-max-element [a-seq]
  (let [helper (fn [v elem]
                 (cond (< elem (get v 0)) (assoc v 0 elem)
                       (> elem (get v 1)) (assoc v 1 elem)
                       :else v))]
    (reduce helper [(first a-seq) (first a-seq)] a-seq)))

;(min-max-element [2 7 3 15 4]) ;=> [2 15]
;(min-max-element [1 2 3 4])    ;=> [1 4]
;(min-max-element [1])          ;=> [1 1]


; EXERCISE 7
(defn insert [sorted-seq n]
  (loop [acc []
         r sorted-seq]
    (cond (empty? r) (conj acc n)
          (< n (first r)) (concat (conj acc n) r)
          :else (recur (conj acc (first r)) (rest r)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

;(insertion-sort [2 5 3 1]) ;=> (1 2 3 5)
;(insertion-sort [1 2])     ;=> (1 2)


; EXERCISE 8
(defn parity [a-seq]
  (let [toggle (fn [a-set
                    elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
  (reduce toggle #{} a-seq)))

;(parity [:a :b :c])    ;=> #{:a :b :c}
;(parity [:a :a :b :b]) ;=> #{}
;(parity [1 2 3 1])     ;=> #{2 3}


; EXERCISE 9
(defn minus
  ([x] (- x))
  ([x y] (- x y)) )

;(minus 2)   ;=> -2
;(minus 4 3) ;=> 1


; EXERCISE 10
(defn count-params [& more] (count more))

;(count-params)            ;=> 0
;(count-params :a)         ;=> 1
;(count-params :a 1 :b :c) ;=> 4


; EXERCISE 11
(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

;(my-*)           ;=> 1
;(my-* 4 3)       ;=> 12
;(my-* 1 2 3 4 5) ;=> 120


; EXERCISE 12
(defn pred-and
  ([] (fn [v] true))
  ([x] (fn [v] (x v))) ;totuusarvo?
  ([x y] (fn [v] (and (x v) (y v))))
  ([x y & more]
   (reduce pred-and (pred-and x y) more)) )

;(filter (pred-and) [1 0 -2])                    ;=> (1 0 -2)
;(filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> (1 7)
;(filter (pred-and number? integer? pos? even?)
;        [1 0 -2 :a 7 "a" 2])                    ;=> (0 2)


; EXERCISE 13 3 points
(defn my-map
  ([f a-seq]
  (let [apu (fn [mmap
                    elem]
                 (concat mmap [(f elem)]))]
   (reduce apu [] a-seq)))
  ([f a-seq b-seq]
   (if (and (seq a-seq) (seq b-seq))
     (cons (f (first a-seq) (first b-seq))
           (my-map f (rest a-seq) (rest b-seq)))
     []))
  ([f a-seq b-seq c-seq]
   (if (and (seq a-seq) (seq b-seq) (seq c-seq))
     (cons (f (first a-seq) (first b-seq) (first c-seq))
           (my-map f (rest a-seq) (rest b-seq) (rest c-seq)))
     []))
  ([f a b c & more]
   ))

(defn map-okay
  "Apufunktio my-map-funktiolle"
  [f a]
  (let [apu (fn [fun]
                 (fn [acc
                      elem]
                   (concat acc [(fun elem)])))
        firsts (reduce (apu first) [] a)
        rests (reduce (apu rest) [] a)]
    (if (some empty? a)
      []
      (cons (apply f firsts)
      (map-okay f rests))
    )))

;(my-map inc [1 2 3 4])                  ;=> (2 3 4 5)
;(my-map + [1 1 1] [1 1 1] [1 1 1])      ;=> (3 3 3)
;(my-map vector [1 2 3] [1 2 3] [1 2 3]) ;=> ((1 1 1) (2 2 2) (3 3 3))
