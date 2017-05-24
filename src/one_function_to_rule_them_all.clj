(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str2 [mjono]
  (str mjono " ")
)

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (str (reduce str (map str2 (reverse (rest (reverse a-seq))))) (last a-seq))))

; TÄMÄ VOI AUTTAA EHKÄ...
; (map str2 ["I" "am" "Legend"]) ;=> ("I " "am " "Legend ")

(defn singleton? [coll]
  (if (empty? coll) 
    false
    (empty? (rest coll))))

(defn new-first [x a-seq]
  (vec (reverse (conj (vec (reverse (rest a-seq))) x))))

(defn my-interpose [x a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (conj (vec (reverse (new-first x (reverse (reduce conj [(first a-seq)] (new-first x a-seq)))))) (last a-seq))))

(defn my-count [a-seq]
  (loop [acc 0
         newseq a-seq]
    (if (empty? newseq)
      acc
      (recur (inc acc) (rest newseq)))))

(defn last-element [a-seq]
  (let [helper (fn [acc a-seq]
    (if (singleton? a-seq)
      (first a-seq)
      (recur (dec acc) (rest a-seq))
    ))]
    (if (empty? a-seq)
      nil
      (helper (count a-seq) a-seq))))

(defn my-reverse [a-seq]
  (last-element a-seq)
  ; (conj (conj [] (last-element [1 2 3 4])) (get [1 2 3 4] (dec (dec (my-count [1 2 3 4])))))
)

(defn min-max-element [a-seq]
  [:-])

(defn insert [sorted-seq n]
  [:-])

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  [:-])

(defn minus 
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params 
  ([] 0)
  ([x] 1)
  ([x y] 2)
;  ([x & more] (+ 1 (my-count more))))
  ([x y & more] (+ 2 (reduce count-params more))))
 
  
(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

; (defn my-map [f seq-1 seq-2]
;   (if (or (empty? seq-1) (empty? seq-2))
;     []
;     (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))
    

(defn my-map
  ([f a-seq] (f a-seq))
  ([f seq1 seq2] (cons (f seq1) (f seq2)))
  ([f seq1 seq2 & more] (reduce my-map (my-map f seq1 seq2) more)))