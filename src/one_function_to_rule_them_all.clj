(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
  (reduce (fn [str1 str2] (str str1 " " str2 )) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (loop [left sorted-seq s '[]]
    (let [e (first left)]
      (cond
        (empty? left)
        (concat s [n])
        (<= n e)
        (concat s [n e] (rest left))
        :else (recur (rest left) (conj s e))))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce (fn [a-seq, e]
            (if (contains? a-seq e)
              (disj a-seq e)
              (conj a-seq e))) #{} a-seq))

(defn minus
  ([x] (- x)) ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-* [& x]
  (reduce * 1 x))

(defn pred-and
  ([] (fn [x] true))
  ([parm] parm)
  ([parm1 parm2] (fn [x] (and (parm1 x) (parm2 x))))
  ([parm1 parm2 & more] (reduce pred-and (pred-and parm1 parm2) more)))

(defn my-map [f & x]
  (if (reduce #(and %1 (empty? %2)) true x) '()
    (cons
     (apply f (reduce #(conj %1 (first %2)) [] x))
     (apply my-map (cons f (reduce #(conj %1 (rest %2)) [] x))))))
