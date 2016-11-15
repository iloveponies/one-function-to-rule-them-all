(ns one-function-to-rule-them-all)

;; concat-elements / Use reduce with concat to concatenate a-seq elements to
;; empty list.
;; concat-elements :: Sequence[a] -> Sequence[a]
(defn concat-elements [a-seq]
  (reduce concat () a-seq))

;; Test cases: concat-elements
(concat-elements [])            ;=> ()
(concat-elements [[1 2]])       ;=> (1 2)
(concat-elements [[1 2] [3 4]]) ;=> (1 2 3 4)


;; str-cat / Reduce the sequence of strings into one string interspersed with
;; spaces.
;; str-cat :: Sequence[String] -> String
(defn str-cat [a-seq]
  (if (empty? a-seq)
      ""
      (reduce (fn [acc foll] (str acc " " foll)) a-seq)))

;; Test cases: str-cat
(str-cat ["I" "am" "Legend"])  ;=> "I am Legend"
(str-cat ["I" "am" "back"])    ;=> "I am back"
(str-cat ["more" " " "space"]) ;=> "more   space"
(str-cat [])                   ;=> ""


;; my-interpose / Insert x between elements of a-seq by reducing it with
;; function that conjs x first to accumulator then the following element.
;; Finally reverses the collection.
;; my-interpose :: (String, Sequence[String]) -> Sequence[String]
(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (reverse (reduce (fn [acc foll] (conj (conj acc x) foll))
                     (list (first a-seq))
                     (rest a-seq)))))

;; Test cases: my-interpose
(my-interpose 0 [1 2 3])               ;=> (1 0 2 0 3)
(my-interpose "," ["I" "me" "myself"]) ;=> ("I" "," "me" "," "myself")
(my-interpose :a [1])                  ;=> (1)
(my-interpose :a [])                   ;=> ()


;; my-count / Empty sequence equals 0. Reduces sequence to integer by
;; incrementing accumulator by 1 until nil value.
;; my-count :: Sequence[a] -> Integer
(defn my-count [a-seq]
  (if (empty? a-seq)
    0
    (reduce (fn [acc foll] (if (nil? foll) acc (inc acc))) a-seq)))

;; Test cases: my-count
(my-count [])      ;=> 0
(my-count [1 2 3]) ;=> 3
(my-count [1])     ;=> 1


;; my-reverse / Use reduce with conj to reverse sequence.
;; my-reverse :: Sequence[a] -> Sequence[a]
(defn my-reverse [a-seq]
  (if (empty? a-seq)
    ()
    (reduce conj (list (first a-seq)) (rest a-seq))))

;; Test cases: my-reverse
(my-reverse [1 2 3]) ;=> (3 2 1)
(my-reverse [1 2])   ;=> (2 1)
(my-reverse [])      ;=> ()


;; min-max-element / Use reduce to go through the list twice and to get minimum
;; and maximum elements. Finally, gather them into a list.
;; min-max-element :: Sequence[Integer] -> [Integer, Integer]
(defn min-max-element [a-seq]
  [(reduce (fn [minm foll] (if (< minm foll) minm foll)) a-seq)
   (reduce (fn [maxm foll] (if (> maxm foll) maxm foll)) a-seq)])

;; Test cases: min-max-element
(min-max-element [2 7 3 15 4]) ;=> [2 15]
(min-max-element [1 2 3 4])    ;=> [1 4]
(min-max-element [1])          ;=> [1 1]


;; insert / Use basic recursion to insert given element to the sorted place.
;; insert :: (Sequence[Integer], Integer) -> Sequence[Integer]
(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) (list n)
    (< n (first sorted-seq)) (cons n sorted-seq)
    :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

;; Test cases: insert
(insert [] 2)      ;=> (2)
(insert [1 3 4] 2) ;=> (1 2 3 4)
(insert [1] 2)     ;=> (1 2)


;; insertion-sort / Use previously made insert function to reduce given
;; sequence to sorted one, starting from empty one.
;; insertion-sort :: Sequence[Integer] -> Sequence[Integer]
(defn insertion-sort [a-seq]
  (if (empty? a-seq)
    ()
    (reduce insert [] a-seq)))

;; Test cases: insert-sort
(insertion-sort [2 5 3 1]) ;=> (1 2 3 5)
(insertion-sort [1 2])     ;=> (1 2)


;; parity / Uses helper function to concatenate only odd frequency elements.
;; The set is formed by reduce with odder to empty list.
;; parity :: Sequence[a] -> Set[a]
(defn parity [a-seq]
  (let [freqs (frequencies a-seq)
        odder (fn [t-seq elem]
                (if (odd? (get freqs elem))
                  (conj t-seq elem)
                  t-seq))]
   (set (reduce odder [] a-seq))))

;; Test cases: parity
(parity [:a :b :c])    ;=> #{:a :b :c}
(parity [:a :a :b :b]) ;=> #{}
(parity [1 2 3 1])     ;=> #{2 3}


;; minus / Two different definitions depending on one or two elements.
;; minus :: Integer / (Integer, Integer) -> Integer
(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

;; Test cases: minus
(minus 2)   ;=> -2
(minus 4 3) ;=> 1


;; count-params / Use '& more' to catch all parameters. Create counter function
;; and finally reduce the parameters with counter.
;; count-params :: List[a] -> Integer
(defn count-params [& more]
  (let [counter (fn [acc elem] (if (nil? elem) acc (inc acc)))]
    (reduce counter 0 more)))

;; Test cases: count-params
(count-params)            ;=> 0
(count-params :a)         ;=> 1
(count-params :a 1 :b :c) ;=> 4


;; my-* / Just gather the parameters to 'more' and use reduce on them with *
;; and initial value 1.
;; my-* :: List[Integer] -> Integer
(defn my-* [& more]
  (reduce * 1 more))

;; Test cases: my-*
(my-*)           ;=> 1
(my-* 4 3)       ;=> 12
(my-* 1 2 3 4 5) ;=> 120


;; pred-and / For the non-trivial case use reduce along with and function.
;; pred-and :: List[Predicate] -> Predicate
(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p & more] (fn [x] (reduce (fn [acc pred] (and acc (pred x))) (p x) more))))

;; Test cases: pred-and
(filter (pred-and) [1 0 -2])                    ;=> (1 0 -2)
(filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> (1 7)
(filter (pred-and number? integer? pos? even?)
        [1 0 -2 :a 7 "a" 2])                    ;=> (0 2)

(defn zip [colls]
  (partition (count colls) (apply interleave colls)))
;(zip [1 2 3] [4 5 6])
(apply partition 1 [[1 2 3]])


;; my-map
(defn my-map [f & seqs]
  (let [zip (fn [colls] (partition (count colls) (apply interleave colls)))
        zip-one (fn [colls] (apply partition 1 colls))
        fun-apply (fn [acc x] (conj acc (apply f x)))]
    (if (== (count seqs) 1)
      (reduce fun-apply [] (zip-one seqs))
      (reduce fun-apply [] (zip seqs)))))

(apply vector (zip [[1 2 3] [1 2 3] [1 2 3]]))

;; Test cases: my-map
(my-map inc [1 2 3 4])                  ;=> (2 3 4 5)
(my-map + [1 1 1] [1 1 1] [1 1 1])      ;=> (3 3 3)
(my-map vector [1 2 3] [1 2 3] [1 2 3]) ;=> ((1 1 1) (2 2 2) (3 3 3))
