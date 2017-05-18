(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [k l] (str k " " l)) a-seq)))

(str-cat ["I" "am" "Legend"])  ;=> "I am Legend"
(str-cat ["I" "am" "back"])    ;=> "I am back"
(str-cat ["more" " " "space"]) ;=> "more   space"
(str-cat [])                   ;=> ""

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (reduce (fn [k l] (if (empty? k)
                        (conj k l)
                        (conj (conj k x) l)))
            []
            a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [counter emp]
                  (inc counter))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    ()
    (reduce (fn [k l] (concat [l] k))
            []
            a-seq)))

(defn min-max-element [a-seq]
  (let [fun (fn [k l] (if (empty? k)
                        (vector l l)
                        (vector (min (int (first k)) l)
                                (max (int (last k)) l))))]
    (reduce fun [] a-seq)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (vector n)
    (if (< n (int (first sorted-seq)))
      (concat [n] sorted-seq)
      (concat (vector (first sorted-seq)) (insert (rest sorted-seq) n)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [fun (fn [k l] (if (contains? k l)
                        (disj k l)
                        (conj k l)))]
    (reduce fun #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x & more]
   (reduce (fn [k l] (inc k)) 1 more)))

(defn my-* [& more]
  (reduce (fn [k l] (* k l)) 1 more))

(defn pred-and
  ([] (fn [i] true))
  ([x] (fn [i] (x i)))
  ([x y] (fn [i] (and (x i) (y i))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn my-map [f a-seq]
  [:-])
