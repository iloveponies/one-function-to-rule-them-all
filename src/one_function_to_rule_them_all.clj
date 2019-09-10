(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (reduce str "" (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [acc elem] (conj acc x elem)) (vector) a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [count elem]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [inserter (fn [coll elem]
                   (conj coll elem))]
    (reduce inserter '() a-seq)))

(defn min-max-element [a-seq]
  (let [finder (fn [acc elem]
                 (vector (min elem (acc 0)) (max elem (acc 1))))]
    (reduce finder (vector (first a-seq) (first a-seq)) a-seq)))

(defn insert [sorted-seq n]
  (loop [result (vector)
         a-seq sorted-seq]
    (cond
     (empty? a-seq)
       (conj result n)
     (>= (first a-seq) n)
       (concat (conj result n) a-seq)
     :else
       (recur (conj result (first a-seq)) (rest a-seq)))))


(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))
        helper (fn [result elem]
                 (toggle result elem))
        ]
    (reduce helper #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-* [& x]
  (let [multiplier (fn [acc numb]
                     (* acc numb))]
    (reduce multiplier 1 x)))

(defn pred-and [& x]
  (fn [k]
    (let [do-checks (fn [checks]
                    (cond
                     (empty? checks)
                       true
                     ((first checks) k)
                       (recur (rest checks))
                     :else
                       false))]
      (if (empty? x)
        true
        (do-checks x)))))

(defn my-map [f & seqs]
  (loop [result (vector)
         seqs seqs]
    (if (empty? (first seqs))
      (seq result)
      (recur (conj result (apply f (map first seqs))) (map rest seqs)))))
