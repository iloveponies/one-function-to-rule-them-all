(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat (list) a-seq))


(defn str-cat [a-seq]
  (apply str (interpose " " a-seq)))

(str-cat ["I" "am" "Legend"])  ;=> "I am Legend"
(str-cat ["I" "am" "back"])    ;=> "I am back"
(str-cat ["more" " " "space"]) ;=> "more   space"
(str-cat [])                   ;=> ""


(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))



(defn my-count [a-seq]
  (reduce (fn [x y] (+ 1 x)) 0 a-seq))

(my-count [])      ;=> 0
(my-count [1 2 3]) ;=> 3
(my-count [1])     ;=> 1

(defn my-reverse [a-seq]
  (reduce (fn [x y] (conj x y)) () a-seq))


(defn min-max-element [a-seq]
  (reduce (fn [x y]
            (if (> (get x 0) y)
              [y (get x 1)]
              (if (< (get x 1) y)
                [(get x 0) y]
                x)))
          (vector (first a-seq) (first a-seq))
          a-seq))


(defn insert [sorted-seq n]
  (loop [pre '()
         s sorted-seq]
    (if (empty? s)
      (concat pre [n])
      (if (>= (first s) n)
        (concat pre (list n) s)
        (recur (concat pre [(first s)]) (rest s))))))



(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce (fn [x y]
            (toggle x y))
          #{}
          a-seq))


(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  [& args] (count args))



(defn my-*
  ([] 1)
  ([x] x)
  ([x & args] (* x (reduce * args))))



(defn pred-and
  ([& args] (fn [x]
              (reduce
                #(and %1 (%2 x))
                true
                args))))

(filter (pred-and) [1 0 -2])                    ;=> (1 0 -2)
(filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> (1 7)
(filter (pred-and number? integer? pos? even?)
        [1 0 -2 :a 7 "a" 2])

(defn my-map
  ([f & cols]))








