(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [space-cat (fn [str-a str-b] (str str-a " " str-b))]
          (reduce space-cat a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (let [insert-between (fn [str-a str-b] (conj str-a x str-b))]
      (reduce insert-between [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [length-counter (fn [n elem]
                         (inc n))]
    (reduce length-counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [add-to-begin (fn [rev-seq elem]
                       (cons elem rev-seq))]
    (reduce add-to-begin [] a-seq)))

(defn min-max-element [a-seq]
  (let [select-extremes (fn [ext-tuple elem]
                          (cond
                            (< elem (first ext-tuple)) [elem (second ext-tuple)]
                            (> elem (second ext-tuple)) [(first ext-tuple) elem]
                            :else ext-tuple))]
                          (reduce select-extremes [(first a-seq) (first a-seq)] (rest a-seq))))


(defn insert [sorted-seq n]
    (let [find-first-index (fn [pred a-seq]
                             (loop [ind 0
                                    rec-seq a-seq]
                               (if (empty? rec-seq)
                                 nil
                                 (if (pred (first rec-seq))
                                   ind
                                   (recur (inc ind) (rest rec-seq))))))
          ind-into-insert (find-first-index #(>= % n) sorted-seq)]
      (if (nil? ind-into-insert)
        (conj sorted-seq n)
        (into [] (concat (subvec sorted-seq 0 ind-into-insert) [n] (subvec sorted-seq ind-into-insert))))))

(defn insertion-sort [a-seq]
  (let [head-sort (fn [head-seq elem]
                    (insert head-seq elem))]
    (reduce head-sort [] a-seq)))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
(reduce toggle #{} a-seq)))

(defn minus
  ([x y] (- x y))
  ([x] (minus 0 x)))[]

(defn count-params
  ([] 0)
  ([x] 1)
  ([x & args] (inc (count args)))) 

(defn my-*
  ([] 1)
  ([x] x)
  ([x & args] (reduce * x args)))

(defn pred-and
  ([] (fn [_] true))
  ([x] #((x %)))
  ;; ([x & args] (let [head-true? (fn [head-bool pred?]
  ;;                                #(and head-bool (fn [z] (pred? z))))]
  ;;               (reduce head-true? #((pred-and x) %) args))))
  ([x & args] (fn [z] (every? identity (map #(% z) (cons x args))))))

(defn my-map [f a-seq & args]
  (let[arg-seq (cons a-seq args)
       sel-from-seqs (fn [g seqs]
                     (reduce (fn [head-first next-elem] (conj head-first (g next-elem))) [] seqs))
       reduce-map (fn [g b-seq] (reduce (fn [head-seq next-elem] (conj head-seq (apply g next-elem))) [] b-seq))
       appliable-seq (if (every? coll? arg-seq)
                       (loop
                           [mapped-head []
                            tail-seqs arg-seq]
                         (if (some empty? tail-seqs) mapped-head
                             (recur (conj mapped-head (sel-from-seqs first tail-seqs)) (sel-from-seqs rest tail-seqs))))
                       (reduce (fn [a b] (conj a (conj nil b))) a-seq))]
    (reduce-map f appliable-seq)))
