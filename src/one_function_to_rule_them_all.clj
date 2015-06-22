(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (let [helper (fn [s e]
                 (conj s e x))]
    (if (empty? a-seq)
      a-seq
      (pop
        (reduce helper [] a-seq)))))

(defn my-count [a-seq]
  (let [helper (fn [count e] 
                 (inc count))]
    (reduce helper 0 a-seq)))

(defn my-reverse [a-seq]
  (let [helper (fn [s e]
                 (conj s e))]
    (reduce helper () a-seq)))

(defn min-max-element [a-seq]
  (let [helper (fn [s e]
                 (cond
                   (< e (first s)) [e (second s)]
                   (> e (second s)) [(first s) e]
                   :else s))]
  (reduce helper [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
    (loop [first-part [], second-part sorted-seq]
      (let [head (first second-part)]
        (cond
          (nil? head)
            (concat first-part [n])
          (<= n head)
            (concat first-part [n head] (rest second-part))
          :else  
            (recur (conj first-part head) (rest second-part)))))))


(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [helper (fn [s e]
                 (if (contains? s e)
                   (disj s e)
                   (conj s e)))]
    (reduce helper #{} a-seq)))

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
    (reduce * (* x y) more)))


(defn pred-and 
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
    (fn [x]
      (loop [preds (concat [p1 p2] more)]
        (if (empty? preds) 
          true
          (if ((first preds) x)
            (recur (rest preds))
            false))))))
(defn my-map
  ([f s] (map f s))
  ([f s & more] (apply map f (cons s more))))
