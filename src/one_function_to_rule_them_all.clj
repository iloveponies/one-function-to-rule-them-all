(ns one-function-to-rule-them-all)

(defn sum [a-seq]
  (if (empty? a-seq)
    0
    (+ (first a-seq)
       (sum (rest a-seq)))))

(defn sum [a-seq]
  (let [sum-helper (fn [acc a-seq]
                     (if (empty? a-seq)
                       acc
                       (recur (+ acc (first a-seq))
                              (rest a-seq))))]
    (sum-helper 0 a-seq)))

(defn sum [a-seq]
  (reduce + 0 a-seq))

(sum [1 2 3 4 5])

(defn product [a-seq]
  (if (empty? a-seq)
    1
    (* (first a-seq)
       (product (rest a-seq)))))

(defn product [a-seq]
  (let [product-helper (fn [acc a-seq]
                         (if (empty? a-seq)
                           acc
                           (recur (* acc (first a-seq))
                                  (rest a-seq))))]))

(defn product [a-seq]
  (reduce * 1 a-seq))

(product [1 2 3 4 5])

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(concat-elements [])            ;=> ()
(concat-elements [[1 2]])       ;=> (1 2)
(concat-elements [[1 2] [3 4]])

(defn seq-min [a-seq]
  (if (empty? a-seq)
    nil
    (reduce min a-seq)))

(seq-min [5 3 2 6])

(seq-min [])

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [spc (fn [a b]
                (str a " " b))]
      (reduce spc a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (let [helper (fn [a b]
                   (cons a (cons x (cons b nil))))]
      (flatten ( reduce helper a-seq)))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (rest  (reduce (fn [a b] (conj a x b)) []  a-seq))))

(my-interpose 0 [1 2 3])               ;=> (1 0 2 0 3)
(my-interpose "," ["I" "me" "myself"]) ;=> ("I" "," "me" "," "myself")
(my-interpose :a [1])                  ;=> (1)
(my-interpose :a [])                   ;=> ()

(defn count-elem [elem a-seq]
  (let [counter (fn [count e]
                  (if (= e elem)
                    (inc count)
                    count))]
    (reduce counter 0 a-seq)))

(count-elem :D [13 "o" :D :$ :D [:D] :< "~^._.^~"])

(defn my-count [a-seq]
  (let [count-helper (fn [count e]
                       (inc count))]
    (reduce count-helper 0 a-seq)))

(defn my-count [a-seq]
  (reduce (fn [c a] (inc c)) 0 a-seq))

(my-count [1 2 3])

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    '()
    (flatten (reduce (fn [a b] (list b a)) a-seq))))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    [nil nil]
    [(reduce min a-seq)
     (reduce max a-seq)]))


(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq) (list n)
   (> n (first sorted-seq))
   (cons (first sorted-seq)
         (insert (rest sorted-seq) n))
   :else
   (cons n sorted-seq)))

(insert [] 2)      ;=> (2)
(insert [1 3 4] 2) ;=> (1 2 3 4)
(insert [1] 2) 

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(insertion-sort [2 5 3 1]) ;=> (1 2 3 5)
(insertion-sort [1 2]) 

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(parity [:a :b :c])
(parity [:a :a :b :b]) ;=> #{}
(parity [1 2 3 1]) 

(defn minus
  ([a] (- a))
  ([a b] (- a b)))

(minus 2)   ;=> -2
(minus 4 3) ;=> 1

(defn max
  ([x] x)
  ([x y] (if (> x y) x y))
  ([x y & more]
     (reduce max (max x y) more)))

(max 1 2 3 4)

(defn count-params [& more]
  (reduce (fn [c a] (inc c)) 0 more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
     (reduce my-* (my-* x y) more)))

(my-*)           ;=> 1
(my-* 4 3)       ;=> 12
(my-* 1 2 3 4 5) ;=> 120

(defn pred-and
  ([] (fn [e] true))
  ([p] p)
  ([p q] (fn [e] (and (p e)
                     (q e))))
  ([p q & more]
     (reduce pred-and (pred-and p q) more)))

(filter (pred-and) [1 0 -2])                    ;=> (1 0 -2)
(filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> (1 7)
(filter (pred-and number? integer? pos? even?)
        [1 0 -2 :a 7 "a" 2])

(number? 0)
(integer? 0)
(pos? 0)
(even? 0)

(defn my-map [f a-seq]
  [:-])
