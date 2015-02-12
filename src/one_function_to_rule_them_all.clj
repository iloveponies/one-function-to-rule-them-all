(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (apply str (reduce #(concat %1 " " %2) a-seq))))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [count elem] (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (let [min-max 
        (fn [mn-mx-vec elem]
          (if (empty? mn-mx-vec) 
            [elem elem]
            (let [[mn mx] mn-mx-vec]  
              (cond 
               (< elem mn) [elem mx]
               (> elem mx) [mn elem]
               :else [mn mx]))))]
    (reduce min-max [] a-seq)))

(defn insert [sorted-seq n]
  (let [first-seq (take-while #(<= % n) sorted-seq)
        rest-seq (drop (count first-seq) sorted-seq)]
    (concat first-seq (list n) rest-seq)))

(defn insertion-sort [a-seq]
  (let [insertion-helper 
        (fn [s elem]
          (insert s elem))]
    (reduce insertion-helper '() a-seq)))

(defn toggle [a-set x]
  (if (a-set x)
    (disj a-set x)
    (conj a-set x)))

(defn parity [a-seq]
  (let [parity-helper
        (fn [a-set elem]
          (toggle a-set elem))]
    (reduce parity-helper #{} a-seq)))

(defn minus 
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-* [& x]
  (reduce * 1 x))

(defn pred-and 
  ([] (fn [x] true))
  ([& p]
   (let [pred-helper
         (fn [pred1 pred2]
           #(and (pred1 %) (pred2 %)))]
     (reduce pred-helper p))))

(defn shorten-seqs [seqs]
  (reduce #(conj %1 (rest %2)) [] seqs))

(defn get-args [seqs]
  (reduce #(conj %1 (first %2)) [] seqs))

(defn any-empty? [seqs]
  (loop [the-seqs seqs]
    (cond 
     (empty? the-seqs) false
     (empty? (first the-seqs)) true
     :else (recur (rest the-seqs)))))

(defn my-map [f & seqs]
  (loop [acc []
         the-seqs seqs]
    (if (any-empty? the-seqs)
      acc
      (recur (conj acc (apply f (get-args the-seqs))) (shorten-seqs the-seqs)))))
