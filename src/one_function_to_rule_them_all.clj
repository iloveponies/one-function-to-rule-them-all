(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [s1 s2] (str s1 " " s2)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [e1 e2] (conj e1 x e2)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [e-count e] (inc e-count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [rev e] (conj rev e)) '() a-seq))

(defn min-max-element [a-seq]
  (let [update (fn [[current-min current-max] elem] [(min elem current-min) (max elem current-max)])]
    (reduce (fn [current elem] (update current elem)) [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (loop [smaller []
         unread sorted-seq]
    (cond
     (empty? unread) (seq (conj smaller n))
     (< n (first unread)) (concat (seq (conj smaller n)) unread)
     :else (recur (conj smaller (first unread)) (rest unread)))))

(defn insertion-sort [a-seq]
  (reduce (fn [sorted-seq elem] (insert sorted-seq elem)) [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
    (reduce (fn [odds elem] (toggle odds elem)) #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& x]
  (reduce (fn [times p] (inc times)) 0 x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))


(defn pred-and
  ([] (fn [_] true))
  ([x] (fn[a] (x a)))
  ([x y] (fn [a] (and (x a) (y a))))
  ([x y & more] (fn [a] ((reduce pred-and (pred-and x y) more) a))))

(defn my-map []
  [:-])
