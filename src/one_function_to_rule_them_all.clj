(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (reduce
   (fn [string elem]
     (cond
      (empty? string) (str elem)
      :else (str string (str " " elem))))
   ""
   a-seq))

(defn my-interpose [x a-seq]
  (reduce
   (fn [acc elem]
     (cond
      (empty? acc) (conj acc elem)
      :else (conj (conj acc x) elem)))
   []
   a-seq))

(defn my-count [a-seq]
  (reduce
   (fn [acc elem]
     (inc acc))
   0
   a-seq))

(defn my-reverse [a-seq]
  (reduce
   (fn [acc elem]
     (cons elem acc))
   '()
   a-seq))

(defn min-max-element [a-seq]
  (let [set-min (fn [acc elem]
                  (cond
                   (= (get acc 0) nil) (assoc acc 0 elem)
                   :else (assoc acc 0 (min elem (get acc 0)))))
        set-max (fn [acc elem]
                  (cond
                   (= (get acc 1) nil) (assoc acc 1 elem)
                   :else (assoc acc 1 (max elem (get acc 1)))))]
  (reduce
   (fn [acc elem]
     (set-max (set-min acc elem) elem))
   [nil nil]
   a-seq)))

(defn insert [sorted-seq n]
  (let [split (split-with (fn [x] (< x n)) sorted-seq)]
    (concat (first split) (list n) (first (rest split)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([& more] (fn [x] ((apply every-pred more) x))))

(defn first-of-all [seqs]
  (reduce
   (fn [acc elem]
     (conj acc (first elem)))
   []
   seqs))

(defn rest-of-all [seqs]
  (reduce
   (fn [acc elem]
     (conj acc (rest elem)))
   []
   seqs))

(defn my-map [f & seqs]
  (seq
    (loop [acc []
           seqs seqs]
      (if (some empty? seqs)
        acc
        (recur (conj acc (apply f (first-of-all seqs))) (rest-of-all seqs))))))
