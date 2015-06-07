(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (or (nil? a-seq) (empty? a-seq))
    ""
    (reduce
     (fn [acc sek]
       (if (= "" acc)
         (str sek)
         (str acc " " sek)))
     ""
     a-seq)))

(defn my-interpose [x a-seq]
  (reduce
   (fn [acc elem]
     (if (empty? acc)
       (conj acc elem)
       (conj (conj acc x) elem)))
   []
   a-seq))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                  (if (nil? e)
                    count
                    (inc count)))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce
   (fn [acc elem]
     (cons elem acc))
   '()
   a-seq))

(defn min-max-element [a-seq]
  (let [mixmax (fn [lohi elem]
                 (if (empty? lohi)
                   [elem elem]
                   (let [[lower upper] lohi]
                     (cond
                      (< elem lower) [elem upper]
                      (> elem upper) [lower elem]
                      :else [lower upper]))))]
    (reduce mixmax [] a-seq)))

(defn insert [sorted-seq n]
  (loop [acc '()
         sek sorted-seq]
    (cond
     (empty? sek) (concat acc (cons n '()))
     (== n (first sek)) (concat acc (cons n sek))
     (< n (first sek)) (concat acc (cons n sek))
     :else (recur (concat acc (cons (first sek) '())) (rest sek)))))


(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce
   (fn [bag elim]
     (if (contains? bag elim)
       (disj bag elim)
       (conj bag elim)))
   #{}
   a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
