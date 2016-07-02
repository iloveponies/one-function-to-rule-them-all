(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (if (empty? a-seq)
    '()
    (reduce #(concat %1 %2) a-seq)))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn- interposer [acc sep item]
  (if (and (sequential? acc) (not (empty? (rest acc))))
    (concat acc [sep item])
    (cons acc [sep item])))

(defn my-interpose [x a-seq]
  (cond
    (empty? a-seq) []
    (empty? (rest a-seq)) a-seq
    :else (reduce #(interposer %1 x %2) a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc x] (+ acc 1)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(conj %1 %2) '() a-seq))

(defn- min-max [mm item]
  (let [f (first mm)
        s (second mm)
        newf (if (< item f) item f)
        news (if (> item s) item s)]
    [newf news]))

(defn min-max-element [a-seq]
  (let [f (first a-seq)]
    (if (empty? a-seq)
      []
      (reduce #(min-max %1 %2) [f f] a-seq))))

(defn stuff [acc toptail bothead n]
  (if (and (not (empty? acc)) (not (apply <= acc)))
    (println toptail bothead n)))

(defn- insorter [acc coll n]
  (let [toptail (last acc)
        bothead (if (empty? coll) n (first coll))]
    (cond
      (every? true? (map empty? [acc coll])) (conj acc n)
      (empty? acc) (recur (conj acc bothead) (rest coll) n)
      (<= n toptail) (concat (concat (drop-last acc) [n toptail]) coll)
      (and (> n toptail) (<= n bothead)) (concat (conj acc n) coll)
      :else (recur (conj acc bothead) (rest coll) n))))

(defn insert
  [sorted-seq n]
  (insorter [] sorted-seq n))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (->> a-seq
       (frequencies)
       (reduce (fn [acc [k v]] (if (odd? v) (conj acc k) acc)) #{})))

(defn minus [& x]
  (apply - x))

(defn count-params [& x]
  (count x))

(defn my-*
  ([]
   1)
  ([& stuff]
   ; not allowed to use apply?...
   (reduce * stuff)))

(defn pred-and [& preds]
  (if (empty? preds)
    (fn [x] true)
    (let [fncs (apply juxt preds)]
      (fn [x]
          (try
            (->> (fncs x)
                 (every? true?))
            (catch java.lang.ClassCastException e false))))))

(defn- chop [acc bod]
  (let [heads (first acc)
        tails (second acc)]
    [(conj heads (first bod)) (conj tails (rest bod))]))

(defn- behead [seqs]
  (reduce chop [[] []] seqs))

(defn- my-mapper [f acc seqs]
  (let [[heads tails] (behead seqs)]
   (cond
     (empty? (first seqs)) acc
     :else (recur f (conj acc (apply f heads)) tails))))

(defn my-map [f & seqs]
  (my-mapper f [] seqs))

