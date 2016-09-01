(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq)) 

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (cond
      (empty? a-seq) '()
      (= 1 (count a-seq)) (seq a-seq)
      :else (rest (reduce
              #(conj %1 x %2) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [x y]
          (if y
            (inc x)
            x))
        0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (reduce
     #(vector
       (min (first %1) %2)
       (max (second %1) %2))
     [(first a-seq)
      (first a-seq)]
     (rest a-seq))))

(defn insert [sorted-seq n]
  (concat (take-while #(< % n) sorted-seq)
          [n]
          (drop-while #(< % n) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce
   (fn [a-set2 x]
     (if (contains? a-set2 x)
       (disj a-set2 x)
       (conj a-set2 x)))
   #{} a-seq))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([& more] (reduce * (first more) (rest more))))

;; (defn pred-and 
;;   ([] (fn [x] true))
;;   ([pred] (fn [x] (pred x)))
;;   ([pred x] (fn [y] (and (pred y) (x y))))
;;   ([pred x & more] (fn [y] ((reduce pred-and (pred-and pred x) more) y))))

(defn pred-and
  ([] #(true))
  ([pred] #(pred %))
  ([pred1 pred2] #(and (pred1 %) (pred2 %)))
  ([pred1 pred2 & preds]
   (reduce pred-and (pred-and pred1 pred2) preds)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
   (fn [x]
     (reduce (fn [v p] (and v (p x)))
             (and (p1 x) (p2 x))
             more))))

(defn my-map
  ([f a-seq]
   (if (empty? a-seq)
     a-seq
     (cons (f (first a-seq)) (my-map f (rest a-seq)))))
  ([f a-seq & more]
   (let [seqs   (cons a-seq more)
         firsts (map first seqs)
         rests  (map rest seqs)]
     (if (some empty? seqs)
       '()
       (cons (apply f firsts) (apply my-map f rests))))))

;; (defn sum [a-seq]
;;   (if(empty? a-seq)
;;     0
;;     (do
;;       (println "first a-seq: " (first a-seq) " + rest " (rest a-seq))
;;       (+ (first a-seq)(sum (rest a-seq))))))

;; ;;; tail recursive sum
;; (defn sum-tail [a-seq]
;;   (let [sum-helper (fn [accum a-seq]
;;                      (if (empty? a-seq)
;;                        accum
;;                        (do
;;                          (println (first a-seq) " + " (rest a-seq) " = " accum)
;;                          (recur (+ accum (first a-seq))
;;                                 (rest a-seq)))))]
;;     (sum-helper 0 a-seq)))

;; (sum [1 2 3 4])

;; (println "sum: "(sum-tail [1 2 3]))
