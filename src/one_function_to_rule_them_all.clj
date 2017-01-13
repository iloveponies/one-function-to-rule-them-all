(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (cond
    (empty? a-seq) ""
    :else (reduce (fn [p n] (str p " " n)) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                 (inc count))]
  (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce #(conj %1 %2) '() a-seq))

(defn min-max-element [a-seq]
  (vector (apply min a-seq) (apply max a-seq)))

(defn insert [sorted n]
  (loop [s []
        sd sorted]
  (cond
    (empty? sd) (conj s n)
    (> (first sd) n) (flatten (conj s n sd))
    :else (recur (conj s (first sd)) (rest sd)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [seq]
  (let [cont (fn [s elem]
          (cond
            (some #(= elem %) s) (disj s elem)
            :else (conj s elem)))]
  (reduce cont #{} seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))


(defn count-params [& args]
  (count args))

(defn my-*
  ([& args]
   (if (zero? (count args)) 1 (reduce * args))))

(defn pred-and
  ([] (constantly true))
  ([x] x)
  ([x y] #(and (x %) (y %)))
  ([x y & args]
   (reduce pred-and (pred-and x y) args)))

(defn my-map [f a-seq]
  [:-])
