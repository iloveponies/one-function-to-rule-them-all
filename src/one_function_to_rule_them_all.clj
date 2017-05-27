(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(clojure.string/join [%1 " " %2]) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj (conj %1 x) %2) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [x _] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) [] a-seq))

(defn min-max-element [a-seq]
  (reduce
    (fn [[prev-smallest prev-largest] x]
      (let [smallest (min x (or prev-smallest x))
            largest (max x (or prev-largest x))]
        [smallest largest])) [] a-seq))

(defn insert [sorted-seq n]
  (concat
    (take-while #(<= % n) sorted-seq)
    [n]
    (drop-while #(<= % n) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity2 [a-seq]
  (reduce (fn [result [key value]]
            (if (odd? value)
              (conj result key)
              result))
          #{}
          (frequencies a-seq)))

(defn parity [a-seq]
  (set (keys (filter (fn [[_ value]] (odd? value)) (frequencies a-seq)))))


(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce #(* %1 %2) (my-* x y) more)))

(defn pred-and2
  ([] (fn [_] true))
  ([& rest]
   (fn [x]
     (reduce
       #(and %1 (%2 x))
       true rest))))

(defn pred-and
  ([] (fn [_] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & rest]
   (reduce
     #(pred-and %1 %2)
     (pred-and p1 p2) rest)))

(defn my-map
  ([f & seqs]
   (loop [seqs seqs
          results []]
     (cond (some empty? seqs)
           results
           :else (let [firsts (reduce #(conj %1 (first %2)) [] seqs)
                       rests (reduce #(conj %1 (rest %2)) [] seqs)
                       result (apply f firsts)]
                   (recur rests (conj results result)))))))

