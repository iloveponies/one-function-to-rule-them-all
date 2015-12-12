(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (let [helper (fn [a] (str " " a))]
    (if (empty? a-seq)
     ""
     (reduce str (first a-seq) (map helper (rest a-seq))))))

(defn my-interpose [x a-seq]
  (let [helper (fn [b] (conj [x] b))]
    (if (empty? a-seq)
      '()
      (seq (reduce concat [(first a-seq)] (map helper (rest a-seq)))))))

(defn my-count [a-seq]
  (let [counter (fn [c e] (inc c))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [helper (fn [a b] (concat [b] a))]
    (if (empty? a-seq)
      '()
      (reduce helper [] a-seq))))

(defn min-max-element [a-seq]
  (let [helper (fn [min-max-acc n]
                 (let [smallest (nth min-max-acc 0)
                       biggest (nth min-max-acc 1)]
                 (cond
                  (< n smallest)
                    [n biggest]
                  (> n biggest)
                    [smallest n]
                  :else
                  min-max-acc)))]
    (reduce helper [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (let [smaller-than-n (fn [k] (> n k))]
  (apply concat [(take-while smaller-than-n sorted-seq)
                 [n]
                 (drop-while smaller-than-n sorted-seq)])))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [occurs-oddly (fn [elem seq]
                       (if (== (mod (count (filter (conj #{} elem) seq)) 2) 1)
                         true
                         false))]
    (loop [s a-seq
           res #{}]
      (cond
       (empty? s)
         res
       (occurs-oddly (first s) a-seq)
         (recur (rest s) (conj res (first s)))
       :else
         (recur (rest s) res)))))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& more] (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and [& more]
  (fn [a] (let [helper (fn [s] (cond
                                (empty? s)
                                  true
                                (not ((first s) a))
                                  false
                                :else
                                  (recur (rest s))))]
             (helper more))))

(defn my-map
  ([f a-seq]
   (loop [res []
          s a-seq]
     (if (empty? s)
       res
       (recur (conj res (f (first s))) (rest s)))))
  ([f a-seq & more]
   (loop [res []
          seqs (into [a-seq] more)]
     (if (some empty? seqs)
       res
       (recur (into res (vector (apply f (map first seqs)))) (map rest seqs))))))
