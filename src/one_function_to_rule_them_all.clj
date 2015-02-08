(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
  (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))


(defn my-count [a-seq]
  (reduce
    (fn [c seq] (inc c))
     0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))


(defn change-element [index other a-seq]
  (loop [index index
         currentIndex 0
         other other
         ret-seq '()
         a-seq a-seq]
    (cond
     (empty? a-seq) (reverse ret-seq)
     (= index currentIndex) (recur index (inc currentIndex) other (conj ret-seq other) (rest a-seq))
     :else (recur index (inc currentIndex) other (conj ret-seq (first a-seq)) (rest a-seq)))))

(defn min-max-element [a-seq]
  (into [] (reduce
   #(cond
      (> 2 (count %1)) (conj (conj %1 %2) %2)
      (> (first %1) %2) (change-element 0 %2 %1)
       (< (first (rest %1)) %2) (change-element 1 %2 %1)
     :else %1)
   [] a-seq)))


(defn insert [sorted-seq n]
  (seq (loop [sorted-seq sorted-seq
         n n
         ret-seq []
         inserted false]
    (cond
    (and (empty? sorted-seq) (false? inserted)) (conj ret-seq n)
    (empty? sorted-seq) ret-seq
    (true? inserted) (recur (rest sorted-seq) n (conj ret-seq (first sorted-seq)) true)
    (<= n (first sorted-seq)) (recur sorted-seq n (conj ret-seq n) true)
    :else (recur (rest sorted-seq) n (conj ret-seq (first sorted-seq)) false))
    )))

(defn insertion-sort [a-seq]
  (reduce #(insert %1 %2) '() a-seq))

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
