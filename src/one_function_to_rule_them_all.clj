(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc s] (str acc " " s)) a-seq )))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [acc e] (if (empty? acc) (conj acc e) (conj (conj acc x) e))) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [count e] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [rev e] (cons e rev)) [] a-seq))

(defn min-max-element [a-seq]
  (let [helper (fn [acc e] (if (empty? acc)
                            [e e]
                            (let [[smin smax] acc ] [(min smin e) (max smax e)])))
        ]
    (reduce helper [] a-seq)))

(defn insert [sorted-seq n]
  (let [helper (fn [x] (< x n))]
    (concat (concat (take-while helper sorted-seq) [n]) (drop-while helper sorted-seq) )))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& xs]
  (count xs))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & preds] (reduce pred-and (pred-and p1 p2) preds)))

(defn firsts
  ( [ seq ] (first seq))
  ( [ seq & seqs ]
      (reduce (fn [ acc a-seq ] (conj acc (first a-seq))) [(first seq)] seqs)))

(defn rests
  ( [ seq ] (rest seq))
  ( [ seq & seqs ]
      (reduce (fn [ acc a-seq ] (conj acc (rest a-seq))) [(rest seq)] seqs)))

(defn emptys?
  ([ seq ] (empty? seq))
  ( [ seq & seqs ]
      (and empty? seqs (reduce (fn [ acc a-seq ] (and acc (empty? a-seq))) (empty? seq) seqs))))

(defn my-map
  ([f coll]
     (do (println coll) ( loop [acc []
                                ss (seq coll)]
                          (if (empty? ss)
                            acc
                            (recur (conj acc (f (first ss))) (rest ss))))))
  ([f coll & colls ]
     (loop [acc []
            ss (conj colls coll)]
       (if (apply emptys? ss)
         acc
         (recur (conj acc (apply f (apply firsts ss))) (apply rests ss))))))
  ;; ([f seq1 seq2]
  ;;    (loop [acc []
  ;;           sseq1 seq1
  ;;           sseq2 seq2]
  ;;      (if (or (empty? sseq1) (empty? sseq2))
  ;;        acc
  ;;        (recur (conj acc (f (first sseq1) (first sseq2))) (rest sseq1) (rest sseq2)))))
  ;; ([f seq1 seq2 & more]
  ;;    (let [helper (fn [acc seq] (my-map f acc seq ))]
  ;;      (reduce helper (f seq1 seq2) more ))))
