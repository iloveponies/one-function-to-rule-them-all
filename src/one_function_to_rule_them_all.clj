(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(concat-elements [])            ;=> ()
(concat-elements [[1 2]])       ;=> (1 2)
(concat-elements [[1 2] [3 4]]) ;=> (1 2 3 4)

(defn concat-strings-whitespace [f s]
  (str f " " s)
  )

(defn str-cat [a-seq]
  (cond (empty? a-seq) ""
        (= 1 (count a-seq)) (str (first a-seq))
        :else (reduce concat-strings-whitespace a-seq)))

(str-cat ["I" "am" "Legend"])  ;=> "I am Legend"
(str-cat ["I" "am" "back"])    ;=> "I am back"
(str-cat ["more" " " "space"]) ;=> "more   space"
(str-cat [])

(interpose 1 [])



(defn my-interpose [x a-seq]
  (interpose x a-seq))

(my-interpose 0 [1 2 3])               ;=> (1 0 2 0 3)
(my-interpose "," ["I" "me" "myself"]) ;=> ("I" "," "me" "," "myself")
(my-interpose :a [1])                  ;=> (1)
(my-interpose :a [])                   ;=> ()

(defn my-count [a-seq]
  (let [f (fn [c e]
            (if (nil? e)
              c
              (inc c)))]
    (reduce f 0 a-seq)))

(my-count [])      ;=> 0
(my-count [1 2 3]) ;=> 3
(my-count [1])     ;=> 1




(defn my-reverse [a-seq]
  (cond
    (<= (count a-seq) 1) a-seq
    :else (reverse a-seq)))

(my-reverse [1 2 3]) ;=> (3 2 1)
(my-reverse [1 2])   ;=> (2 1)
(my-reverse [])      ;=> ()

(defn min-max-element [a-seq]
  (let [sorted-seq (sort a-seq)]
    [(nth sorted-seq 0) (nth sorted-seq (- (count sorted-seq) 1))]))

(min-max-element [2 7 3 15 4]) ;=> [2 15]
(min-max-element [1 2 3 4])    ;=> [1 4]
(min-max-element [1])          ;=> [1 1]

(defn insert [sorted-seq n]
  (let [f (first sorted-seq)]
    (cond
      (nil? f) (cons n '())
      (<= n f) (cons n sorted-seq)
      :else (cons f (insert (rest sorted-seq) n)))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
  (disj a-set elem)
  (conj a-set elem)))


(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(parity [:a :b :c])    ;=> #{:a :b :c}
(parity [:a :a :b :b]) ;=> #{}
(parity [1 2 3 1])     ;=> #{2 3}

(defn minus
  ([x] (- x))
  ([x y] (- x y))
  )

(defn count-params
  ([] 0)
  ([x] 1)
  ([x y] 2)
  ([x y & more]
   (+ 2 (count more))))


(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn all-true [fns arg]
  (let [f (first fns)]
    (if (nil? f)
      true
      (and (f arg) (all-true (rest fns) arg)))
    )
  )

(defn pred-and
  ([] (fn [z] true))
  ([x] x)
  ([x y] (fn [z] (and (x z) (y z))))
  ([x y & more]
   (fn [z] (and (x z)
                (y z)
                (all-true more z)))))

(defn multi [f s]
  (let [[a b c d e f g h] s])
  (map f s))

(defn multi-map [f s]
  (if (= 1 (count s))
    (first s)
    (map f (first s) (multi-map f (rest s))))
  )

(defn cleaner [a-seq]
  (if (empty? a-seq)
    '()
    (let [f (first a-seq)
          r (rest a-seq)]
      (if (vector? f)
        (cons (flatten f) (cleaner r))
        (cons f (cleaner r)))))
  )

(defn my-map
  ([f s1] (map f s1))
  ([f s1 & more]
   (cleaner (multi-map f (cons s1 more)))
   ))



(my-map inc [1 2 3 4])                  ;=> (2 3 4 5)
(my-map + [1 1 1] [1 1 1] [1 1 1])      ;=> (3 3 3)
(my-map vector [1 2 3] [1 2 3] [1 2 3]) ;=> ((1 1 1) (2 2 2) (3 3 3))
;(my-map vector [1 2 3] [1 2 3] [1 2 3] [1 2 3])



