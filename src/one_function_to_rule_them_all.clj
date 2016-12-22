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
  (let [ counter (fn [count e]
                   (inc count)) ]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [ helper (fn [reversed e]
                  (cons e reversed)) ]
    (reduce helper [] a-seq)))

(defn min-max-element [a-seq]
  (let [ helper (fn [min-max e]
                  (if (empty? min-max) 
                    [e e]
                    (do
                      (cond 
                        (< e (first min-max)) [e (last min-max)]
                        (> e (last min-max)) [(first min-max) e]
                        :else min-max)))) ]
    (reduce helper [] a-seq)))
                    

(defn insert [sorted-seq n]
  (if (empty? sorted-seq) 
    (list n)
    (do
      (loop [counter 0
             orig-seq sorted-seq 
             tmp-seq [] ]
        (if (and (not (empty? orig-seq))(> n (first orig-seq)))
          (recur (inc counter) (rest orig-seq) (conj tmp-seq (first orig-seq)))
          (concat tmp-seq (conj orig-seq n)))))))

(defn insertion-sort [a-seq]
  (if (empty? a-seq)
    a-seq
    (do 
      (reduce insert [] a-seq))))

(defn parity [a-seq]
  [:-])

(defn minus [x]
  :-)

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
