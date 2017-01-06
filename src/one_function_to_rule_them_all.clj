(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (cond 
    (empty? a-seq)
      ""
    :else
      (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (cond
    (empty? a-seq)
      a-seq 
    :else
    (reduce (fn [a b] (conj (conj a x) b)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (cond
    (empty? a-seq)
      0
    :else
      (reduce (fn [a _] (inc a)) a-seq )))

(defn my-reverse [a-seq]
  (cond
    (empty? a-seq)
     '()
    :else
     (reduce conj '() a-seq)))

(defn min-max-element [a-seq]
  (let [checker (fn [a b]
                  (assoc 
                    (assoc a 0 (min (get a 0) b)) 
                      1 (max (get a 1) b)))] 
  (reduce checker [(first a-seq) (first a-seq)] (rest a-seq))))

;; todo: this should be a bit cleaner...
(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq)
      [n]
    (< n (first sorted-seq))
      (concat [n] sorted-seq)
    :else 
    (loop [before [(first sorted-seq)]
           after (rest sorted-seq)]
          (cond
            (empty? after)
              (concat before [n])
            (and (> n (last before)) (< n (first after)))
              (concat before [n] after)
            :else
              (recur (conj before (first after)) (rest after))))))

(defn insertion-sort [a-seq]
  (cond
    (empty? a-seq)
     a-seq
    :else
    (reduce insert [] a-seq)))

(defn parity [a-seq]
  (let [toggle (fn [a-set item]
                 (if (contains? a-set item)
                   (disj a-set item)
                   (conj a-set item)))]
  (reduce toggle #{} a-seq)))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& more] 
  (cond
    (zero? (count more) )
      1
    (== 1 (count more))
      (first more)
    :else
     (reduce * more)))

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
