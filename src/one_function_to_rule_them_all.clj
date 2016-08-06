(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce (fn [c y]
            (concat c y))
          []
          a-seq))

;; (concat-elements [])            ;=> ()
;; (concat-elements [[1 2]])       ;=> (1 2)
;; (concat-elements [[1 2] [3 4]]) ;=> (1 2 3 4)


(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
  (let [[ start  & end ] a-seq]
  (reduce (fn [c string]
            (str  c " " string))
          start
          end ))))

;; (str-cat ["I" "am" "Legend"])  ;=> "I am Legend"
;; (str-cat ["I" "am" "back"])    ;=> "I am back"
;; (str-cat ["more" " " "space"]) ;=> "more   space"
;; (str-cat [])                   ;=> ""


(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
  (let [ [start & end] a-seq]
    (reduce (fn [c value]
           (conj c x value))
            [start]
           end))))

;; (my-interpose 0 [1 2 3])               ;=> (1 0 2 0 3)
;; (my-interpose "," ["I" "me" "myself"]) ;=> ("I" "," "me" "," "myself")
;; (my-interpose :a [1])                  ;=> (1)
;; (my-interpose :a [])                   ;=> ()

(defn my-count [a-seq]
  (reduce (fn [c x]
            (inc c))
            0
            a-seq))

;; (my-count [])      ;=> 0
;; (my-count [1 2 3]) ;=> 3
;; (my-count [1])     ;=> 1



(defn my-reverse [a-seq]
  (if (empty? a-seq)
    ()
  (reduce (fn [c elt]
          (concat [elt] c))
          ()
          a-seq)))

;; (my-reverse [1 2 3]) ;=> (3 2 1)
;; (my-reverse [1 2])   ;=> (2 1)
;; (my-reverse [])      ;=> ()


(defn helper-pred [pred  min-c min-elt]
  (if (pred min-elt min-c)
    min-elt
    min-c))

(defn min-max-element [a-seq]
  (let [ [start & end] a-seq]
    (reduce (fn [c elt]
              (let [ [min-c max-c] c
                     new-min (helper-pred < elt min-c)
                     new-max (helper-pred > elt max-c)]
                [new-min  new-max]))
            [start start]
            end)))

;; (min-max-element [2 7 3 15 4]) ;=> [2 15]
;; (min-max-element [1 2 3 4])    ;=> [1 4]
;; (min-max-element [1])          ;=> [1 1]


(defn insert [sorted-seq n]
  (let [ start (take-while #(< % n) sorted-seq)
         end (drop (count start) sorted-seq)]
       (concat start [n] end)))

;; (insert [] 2)      ;=> (2)
;; (insert [1 3 4] 2) ;=> (1 2 3 4)
;; (insert [1] 2)     ;=> (1 2)


(defn insertion-sort [a-seq]
  (reduce (fn [c elt]
            (insert c elt))
          []
          a-seq))

;; (insertion-sort [2 5 3 1]) ;=> (1 2 3 5)
;; (insertion-sort [1 2])     ;=> (1 2)


(defn counter [liste elt]
  (count (filter #(= % elt) liste)))

;; (counter [ 1 1 2 ] 1) ;=>2
;; (counter [ 1  2 ] 1) ;=>1


(defn parity [a-seq]
  (reduce (fn [ c elt]
            (let [ number (counter a-seq elt)]
              (if (odd? number)
                (conj c elt)
                c)))
          #{}
          (set a-seq)))


;; (parity [:a :b :c])    ;=> #{:a :b :c}
;; (parity [:a :a :b :b]) ;=> #{}
;; (parity [1 2 3 1])     ;=> #{2 3}


(defn minus
([x] (* -1 x))
([x y] (- x y)))

;; (minus 2)   ;=> -2
;; (minus 4 3) ;=> 1


(defn count-params [& arg ]
    (count arg))

;; (count-params)            ;=> 0
;; (count-params :a)         ;=> 1
;; (count-params :a 1 :b :c) ;=> 4


(defn my-* [ & arg ]
  (let [ number-arg (count arg)]
  (cond
    (= 0 number-arg ) 1
    :else (apply * arg))))

;; (my-*)           ;=> 1
;; (my-* 4 3)       ;=> 12
;; (my-* 1 2 3 4 5) ;=> 120


(defn pred-and [& arg]
  (fn [x]
    (reduce (fn [c pred]
              (and c
                   (pred x)))
              true
              arg)))


;; (filter (pred-and) [1 0 -2])                    ;=> (1 0 -2)
;; (filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> (1 7)
;; (filter (pred-and number? integer? pos? even?)[1 0 -2 :a 7 "a" 2]);=> (2)


(defn helper-vector [i a-seq]
  (map #(get % i) a-seq))


(defn my-vectors [  a-seq ]
  (map (fn [i]
         (helper-vector i a-seq ))
       (range (count (first a-seq)))))


(defn my-map [f  & arg]
  (let [ vectors (my-vectors arg)
         map-vectors (map #(apply f %) vectors)]
    map-vectors))

;;  (my-map inc [1 2 3 4])                  ;=> (2 3 4 5)
;;  (my-map  + [1 1 1] [1 1 1] [1 1 1])         ;=> (3 3 3)
;;  (my-map vector [1 2 3] [1 2 3] [1 2 3]) ;=> ((1 1 1) (2 2 2) (3 3 3))
