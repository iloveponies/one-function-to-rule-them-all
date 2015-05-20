(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (cond
   (< (count a-seq) 2)
     (str (first a-seq))
   :else
    (reduce (fn [string se]
            (str string " " se)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce (fn [acc it]
            (if (empty? acc)
              (seq [it])
              (concat acc [x it]))) [] a-seq))


(defn my-count [a-seq]
  (if (empty? a-seq)
    0
    (reduce (fn [n _]
              (inc n)) a-seq)))

(defn my-reverse [a-seq]
  (reduce (fn [acc it]
            (cons it acc)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [acc item]
            (if (nil? item)
              acc
              (if (< item (get acc 0))
                (assoc acc 0 item)
                (if (> item (get acc 1))
                  (assoc acc 1 item)
                  acc)))) [(first a-seq) (first a-seq)] a-seq))


(defn insert [sorted-seq n]
  (let [s (apply list sorted-seq)]
    (cond
      (empty? s)
       (list n)
      (> (first s) n)
       (conj s n)
      :else
       (conj (insert (rest s) n) (first s)))))

(defn insertion-sort [a-seq]
  (reduce (fn [sorted n]
          (if (nil? n)
            sorted
            (insert sorted n))) '() a-seq))


(defn parity [a-seq]
  (reduce (fn [acc-set n]
          (cond
           (nil? n)
             acc-set
           (contains? acc-set n)
             (disj acc-set n)
           :else
             (conj acc-set n))) #{} a-seq))


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
