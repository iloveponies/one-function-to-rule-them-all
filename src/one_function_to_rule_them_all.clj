(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str "" (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (drop 1 (reduce (fn [accum elem] (conj accum x elem)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [cnt _] (inc cnt)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [accum el] (cons el accum)) [] a-seq))

(defn min-max-element [a-seq]
  (reduce
    (fn [mm el] (if (empty? mm)
                  [el el]
                  (let [[mm mx] mm]
                    [(min mm el) (max mx el)])))
    []
    a-seq))

(defn insert [sorted-seq n]
  (let [pred (fn [el] (>= n el))]
    (concat
      (take-while pred sorted-seq)
      [n]
      (drop-while pred sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce
    (fn [accum el] (if (contains? accum el)
                     (disj accum el)
                     (conj accum el)))
      #{}
    a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x & more] (reduce * x more)))

; eats collection of predicates and applies "and" giving a combined predicate fn
(defn pred-and [& more]
  (fn [elem] (reduce
               (fn [accum-result pred] (and accum-result (pred elem)))
               true
               more)))

; documenting my impl for myself...
;
; 1234.
; 123..
; 12345
; 1st vertical slice [1 1 1], tails [(2 3 4) (2 3) (2 3 4 5)], we apply f on slice
; result becomes a 1st elem in final collection.
; premature ending of one of the seqs on 3rd element should stop recursion

(defn my-map [f & seqs]
  (let [slicer (fn [[slice' rests'] seqs'] ; collecting vertical slice and sequences tails for next iteration
                 (if (empty? seqs')
                   [slice' rests'] ; all sequences finished at once
                   (if (empty? (first seqs'))
                     [[] []] ; some of the sequences ended prematurely
                     (recur
                       [(conj slice' (ffirst seqs')) (conj rests' (rest (first seqs')))] ; augment vertical slice and sequences tails
                       (rest seqs')))))
        [slice rests] (slicer [[] []] seqs)]
    (cond
      (empty? slice) [] ; was premature end of one of the sequences - stop recursion
      (empty? rests) [(apply f slice)] ; base case
      :else (cons (apply f slice) (apply my-map (cons f rests)))))) ; recursively cons resulting map