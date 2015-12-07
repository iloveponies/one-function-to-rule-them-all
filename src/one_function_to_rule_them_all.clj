(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq)
            )))

(defn my-interpose [x a-seq]
;  (cond
;    (empty? a-seq) '()
;    (empty? (rest a-seq)) a-seq
;    :else (let
;              [interpose-helper
;                (fn [elem]
;                  (conj (cons elem nil) x))]
;            (reduce interpose-helper '() a-seq))))
  (rest (apply concat (map (fn [elem] (conj (cons elem nil) x)) a-seq))))

(defn my-count [a-seq]
   (let [counter (fn [count]
                   (inc count))]
     (reduce counter 0 a-seq)))

(defn count-elem [elem a-seq]
  (let [counter (fn [count e]
                  (if (= e elem)
                    (inc count)
                    count))]
    (reduce counter 0 a-seq)))


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
