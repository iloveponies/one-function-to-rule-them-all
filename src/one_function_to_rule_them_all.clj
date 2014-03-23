(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [s1 s2] (if s1 (str s1 " " s2) s2)) nil a-seq)))

(defn my-interpose [x a-seq]
  (cond
   (empty? a-seq) '()
   (= 1 (count a-seq)) a-seq
   :else (reduce (fn [e1 e2] (if (nil? e1) (list e2) (concat e1 (list  x e2)))) nil a-seq)))

(defn my-count [a-seq]
  (reduce (fn [s e]
            (+ 1 s)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [l e]
            (conj l e)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[minim maxim] e] [(min minim e) (max maxim e)]) [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (list n)
    (loop [prefix '()
           seq sorted-seq]
      (let [done (if (first seq) ())])
      (cond
       (or (empty? seq) (nil? (first seq))) (concat prefix (list  n))
       (< n (first seq)) (concat  prefix (list  n) seq)
       :else (recur (concat prefix (list  (first seq))) (rest  seq))))))

(defn insertion-sort [a-seq]
  (reduce (fn [sorted-list e] (insert sorted-list e))
          '()
          a-seq))

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))

(defn parity [a-seq]
  (reduce (fn [result e] (toggle result e))
          #{}
          a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-* [& vals]
  (reduce (fn [prod val] (* prod val))
          1
          vals))

(defn pred-and [& preds]
  (fn [x]
    ((reduce (fn [func pred] (fn [x] (and (func x) (pred x))))
             (fn [x] true)
              preds)
     x)))

(defn my-map [f & seqs]
  (loop [res []
         seqs seqs]
    (if (reduce (fn [any-empty seq] (or any-empty (empty? seq)))
                false
                seqs)
      res
      (recur (conj res (apply f (map first seqs))) (map rest seqs)))))
