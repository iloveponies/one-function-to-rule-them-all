(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (clojure.string/join " " a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (if (= (count a-seq) 1)
      (seq a-seq)
      (rest (reduce #(conj %1 x %2) [] a-seq)))))

(defn my-count [a-seq]
 (loop [counter 0
        a-seq a-seq]
    (if (empty? a-seq)
      counter
      (recur (inc counter) (rest a-seq)))))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [minim (reduce min a-seq)
        maxim (reduce max a-seq)]
    [minim maxim]))

(defn insert [sorted-seq n]
  (sort (conj sorted-seq n)))

(defn insertion-sort [a-seq]
  (reduce insert nil a-seq))

(defn parity [a-seq]
  (set
   (mapcat
    (fn [[k v]]
      (when (odd? v) [k]))
    (reduce (fn [result item]
              (update-in result [item] (fnil inc 0)))
            {}
            a-seq))))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce my-* (my-* x y) more)))

(defn andp [& fns]
      (fn [& args]
        (every? #(apply % args) fns)))

(defn pred-and
      ([] (fn [x] true))
      ([x] x)
      ([x y] (andp x y))
      ([x y & more]
          (reduce pred-and (pred-and x y) more)))

(defn my-map
  ([f coll] (seq (reduce #(conj %1 (f %2)) [] coll)))
  ([f coll & colls]
    (let [colls (cons coll colls)]
      (my-map (partial apply f)
              (partition (count colls)
              (apply interleave colls))))))
