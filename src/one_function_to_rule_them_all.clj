(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))



(concat [[1 2] [3 4]])

(concat [1 2])

(concat-elements [])            ;=> ()
(concat-elements [[1 2]])       ;=> (1 2)
(concat-elements [[1 2] [3 4]]) ;=> (1 2 3 4)



(str "foo" " " (str " " "foo"))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))


(str-cat ["foo" "fuu" "foo" " " "faa"])
(str-cat [])

((fn [x y] (str x " " y)) "foo" "fuu")

(conj [1] 0 0)


(cons 1 '())

(defn my-interpose [x a-seq]
  (if (>= 1 (count a-seq))
    (concat a-seq)
    (concat (rest (reduce (fn [a b] (conj a x b)) [] a-seq)))))


(my-interpose 0 [1 2 3])
(my-interpose "," ["I" "me" "myself"]) ;=> ("I" "," "me" "," "myself")
(my-interpose :a [1])                  ;=> (1)
(my-interpose :a [])                   ;=> ()

(+ 1 (+ 1))


(inc 1)

(defn my-count [a-seq]
  (let [helper (fn [k e]
                 (inc k))]
    (reduce helper 0 a-seq)))

(conj '(1 2) 3)

(my-count [1 2 3])
(my-count ["f" "a" "b" ""])

(my-count [])

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(my-reverse  [])

(defn min-max-element [a-seq]
  (let [a (reduce min a-seq)
        b (reduce max a-seq)]
    [a b]))

(min-max-element [6 8 9 1 4])



(concat [1 2 3] [4 5])

(defn insert [sorted-seq n]
  (loop [after sorted-seq
         before []]
    (cond
      (empty? after) (concat (conj before n))
      (<= n (first after)) (concat (conj before n) after)
      :else (recur (rest after) (conj before (first after))))))



(insert '(1 2 3 5 6) 8)
(insert [] 2) 
(insert [1 3 4] 2) ;=> (1 2 3 4)
(insert [1] 2)     ;=> (1 2)

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))



(insertion-sort [2 354 5 1])
(insertion-sort [1 2])




(defn parity [a-seq]
  (let [toggle (fn [xs x] (if (contains? xs x)
                            (disj xs x)
                            (conj xs x)))]
    (reduce toggle #{} a-seq)))

(parity [:a :a :b :b])
(parity [:a :b :c])

(parity [1 2 3 1])


(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(minus 2)

(minus 4 2)


(defn count-params [& xs]
  (let [helper (fn [k e]
                 (inc k))]
    (reduce helper 0 xs)))

(count-params "" "" "" "")


(defn my-* [& xs]
  (reduce * 1 xs))

(my-* 2 2 2 2)


(defn pred-and
  ([p1] (fn [x] (p1 x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x)))) 
  ([p1 p2 & preds]
     (reduce pred-and (pred-and p1 p2) preds)))



(cons (vector 2) (cons (vector 1) '()))



; jos mapilla vain yksi sekvenssi niin sitten palauta sama sekvenssi, mutta jokainen jäsen funktiolla prosessoituna

(defn my-map
  ([f xs] ()


  


   
