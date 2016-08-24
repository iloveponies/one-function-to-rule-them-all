(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce
     (fn [sana1 sana2] (str sana1 " " sana2)) a-seq)))

(defn my-interpose [x a-seq]
  (loop [sekvenssi a-seq
         eka (first sekvenssi)
         loput (rest sekvenssi)
         tulos []]
    (cond
     (empty? sekvenssi) (sequence tulos)
     (empty? loput) (seq (conj tulos eka))
     :else (recur loput
                  (first loput)
                  (rest loput)
                  (conj (conj tulos eka) x)))))

(defn my-count [a-seq]
  (let [apuri (fn [summa elem]
                  (inc summa))]
    (reduce apuri 0 a-seq)))

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
