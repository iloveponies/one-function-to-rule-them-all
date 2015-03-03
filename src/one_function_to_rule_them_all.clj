(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(concat-elements [[1 2]])
(concat-elements [[1 2] [3 4]])

(defn str-cat [a-seq]
  (if (empty? a-seq)
    (str "")
  (reduce (fn [a b] (apply str [a " " b])) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [res a] (if (empty? res) [a]
                          (conj res x a))) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [c i] (inc c)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc e] (concat [e] acc)) [] a-seq))

(defn min-max-element [a-seq]
  (let [min-max (fn [acc e]
            (let [left (get acc 0)
                  right (get acc 1)]
              [(min e left) (max e right)]))]

  (reduce min-max [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (let [c (fn [e] (< e n))
        beginning (take-while c sorted-seq)
        end (drop-while c sorted-seq)]
    (concat beginning [n] end)))

(defn insertion-sort [a-seq]
  (reduce (fn [acc e]
            (insert acc e)) [] a-seq))

(defn toggle [a-set e]
  (if (contains? a-set e)
    (disj a-set e)
    (conj a-set e)))


(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
    (count x))

(defn my-* [& more]
  (if (empty? more) 1
    (reduce * 1 more)))

(defn pred-and [& preds]
  (if (empty? preds) (fn [e] true)
    (fn [e] (every?
             (fn [r] (= r true))
             (map (fn [p] (p e))
                  preds)))))

(defn runmap [f a-seq]
  (if (empty? a-seq)
    ()
  (cons (f (first a-seq)) (runmap f (rest a-seq)))))

(defn reorder [seqs]
  (if (every? empty? seqs)
    []
    (cons (runmap first seqs) (reorder (runmap rest seqs)))))

(defn my-map
  ([f a-seq] (runmap f a-seq))
  ([f a-seq & more]
   (let [maps (cons a-seq more)
         reordered (reorder maps)]
   (runmap
    (fn [s] (apply f s))
    reordered))))

(my-map vector [[1 2 3] [1 2 3] [1 2 3]])
