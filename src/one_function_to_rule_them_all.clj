(ns one-function-to-rule-them-all)

; (concat-elements [[1 2] [3 4]]) ;=> (1 2 3 4)
(defn concat-elements [a-seq]
  (reduce concat () a-seq))

; (str-cat ["I" "am" "Legend"])  ;=> "I am Legend"
(defn str-cat [a-seq] 
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

; (my-interpose "," ["I" "me" "myself"]) ;=> ("I" "," "me" "," "myself")
(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  :-)

(defn my-reverse [a-seq]
  [:-])

(defn min-max-element [a-seq]
  [:-])

(defn insert [sorted-seq n]
  [:-])

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  [:-])

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))


(defn count-params [ & more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)  
  ([x y] (* x y))
  ([x y & more]
   (let [vals (-> more
                  (conj x)
                  (conj y)) ]
   (reduce * vals))))

(defn pred-and
  ([] true)
  ([pred] (fn [x] (pred x)))
  ([pred1 pred2] (fn [x] (if (and (pred1 x) (pred2 x))
                           true
                           false )))
  ([pred1 pred2 & more] (fn [x] (let [all_preds (-> more
                                                    (conj pred1)
                                                    (conj pred2)) ]
                                (empty? (filter false? (map #( % x) all_preds)))))))

; (filter (pred-and number? integer? pos? even?)  [1 0 -2 :a 7 "a" 2])
; (filter (pred-and number? integer? pos? even?)  [1 0 -2 7 2])
;  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
