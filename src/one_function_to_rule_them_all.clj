(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))


(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (if (empty? a-seq)
    0
    (reduce (fn [counter _e] (inc counter)) a-seq)))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    '()
    (reduce #(cons %2 %1) [(first a-seq)] (rest a-seq))))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [[min max] item]
              (cond
                (< item min) [item max]
                (> item max) [min item]
                :else [min max]))
            [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (let [idx (loop [i 0]
              (if (or (> i (dec (count sorted-seq))) (< n (get (vec sorted-seq) i)))
                i
                (recur (inc i))))]
    (concat (take idx sorted-seq) [n] (drop idx sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert [(first a-seq)] (rest a-seq)))

(defn parity [a-seq]
  (set (keys (filter #(odd? (second %)) (reduce (fn [result item]
                                                  (update-in result [item] #(if (nil? %) 1 (inc %))))
                                                 {}
                                                 a-seq)))))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-* [& x]
  (let [n (count x)]
    (cond (= 0 n) 1
          (= 1 n) x
          (= 2 n) (* (first x) (second x))
          (< 2 n) (reduce * x))))

(defn pred-and [& x]
  (let [n (count x)]
    (cond (= 0 n) (constantly true)
          (= 1 n) x
          (= 2 n) #(and ((first x) %) ((second x) %))
          (< 2 n) (fn [param] (every? true? (map #(% param) x))))))

(defn my-map
  ([f seq-1]
   (if (seq seq-1)
     (cons (f (first seq-1)) (my-map f (rest seq-1)))))
  ([f seq-1 seq-2]
   (if (and (seq seq-1) (seq seq-2))
     (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))
  ([f seq-1 seq-2 seq-3]
   (if (and (seq seq-1) (seq seq-2) (seq seq-3))
     (cons (f (first seq-1) (first seq-2) (first seq-3)) (my-map f (rest seq-1) (rest seq-2) (rest seq-3)))))
  ([f seq-1 seq-2 seq-3 & seqs]
   (if (and (seq seq-1) (seq seq-2) (seq seq-3) (every? seq seqs))
     (let [helper (fn helper [ss]
                    (if (not-any? empty? ss)
                      (cons (my-map first ss) (helper (my-map rest ss)))))]
       (my-map #(apply f %) (helper (conj seqs seq-1 seq-2 seq-3)))))))