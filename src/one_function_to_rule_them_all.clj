(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))


(defn str-cat [a-seq]
(if (or (empty? a-seq) (= 1 (count a-seq)))
  ""
  (reduce str (interpose " " (concat a-seq)))))

(defn my-interpose [x a-seq]
  (if (or (empty? a-seq) (= 1 (count a-seq)))
  a-seq
  (flatten (reduce (fn [acc next]
            (conj [acc] [x] [next])) a-seq))))


(defn my-count [a-seq]
  (if (empty? a-seq)
    0
    (reduce (fn [c _]
              (inc c)) 0 a-seq)
    ))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  (vector (reduce min a-seq)
          (reduce max a-seq)))

(defn insert [sorted-seq n]
   (concat (filter #(< % n) sorted-seq)
           [n]
           (filter #(> % n) sorted-seq)))

(defn insertion-sort [a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [aseq k]
              (insert aseq k)) '() a-seq)))

(defn parity [a-seq]
 (set (reverse (reduce (fn [aseq k]
                (if (some #(= % k) aseq)
                 (remove #(= % k) aseq)
                 (conj aseq k))) () a-seq))))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& args]
  (reduce (fn [x _]
            (inc x)) 0 args))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & args] (reduce * (* x y) args)))

(defn pred-and
  ([] (fn [z]
        (= z z)))
  ([p] (fn [z]
         (p z)))
  ([pred1 pred2] (fn [z]
           (and
            (pred1 z)
            (pred2 z))))
  ([x y & preds] (fn [z]
                   (reduce
                    #(and %1 (%2 z))
                    (and
                     (x z)
                     (y z))
                    preds))))

(defn my-map [f a-seq]
  [:-])
