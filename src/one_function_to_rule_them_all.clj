(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc x]
              (str acc " " x))
            a-seq)))

(defn my-interpose [x a-seq]
  (let [s (reverse a-seq)] 
    (if (empty? s)
      s
      (reduce (fn [acc n] 
                (conj (conj acc x) n))
              (seq [(first s)])
              (rest s)))))

(defn my-count [a-seq]
  (if (empty? a-seq) 
    0
    (reduce (fn [acc x]
              (if (nil? x)
                acc
                (inc acc)))
            a-seq)))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [acc x]
              (conj acc x))
            (seq [(first a-seq)])
            (rest a-seq))))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    nil
    (reduce (fn [acc x]
              (let [[min max] acc]
                (cond
                 (< x min) [x max]
                 (> x max) [min x]
                 :else [min max])))
            [(first a-seq) (first a-seq)]
            a-seq)))

(defn insert [sorted-seq n]
  (let [find-ind (fn [a-seq]
                   (loop [i 0
                          s a-seq]
                     (cond
                      (= nil (first s)) i
                      (> (first s) n) i
                      :else (recur (inc i) (rest s)))))
        place (find-ind sorted-seq)]

    (concat (take place sorted-seq) [n] (drop place sorted-seq))))


(defn insertion-sort [a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce insert [(first a-seq)] (rest a-seq))))

(defn parity [a-seq]
  (let [toggle (fn [c e]
                 (if (contains? c e)
                   (disj c e)
                   (conj c e)))]
    (reduce toggle #{} a-seq)))

(defn minus 
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and 
  ([] (fn [& more] true))
  ([p] p)
  ([pa pb] (fn [x] (and (pa x) (pb x))))
  ([pa pb & more] (reduce pred-and (pred-and pa pb) more)))

(defn my-map
  ([f a-seq] (seq (reduce #(conj %1 (f %2)) [] a-seq)))
  ([f a-seq & more]
   (loop [acc []
          seqs (cons a-seq more)]
     (if (empty? (first seqs))
       (seq acc)
       (recur (conj acc (apply f (my-map first seqs))) (my-map rest seqs))))))
