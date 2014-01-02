(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn magic [x z]
  (reverse (conj (conj '() x) z)))

(defn my-interpose3 [z a-seq]
  (reduce
   (fn [acc x]
     (reduce conj acc (magic z x))) (conj '() (first a-seq)) (rest a-seq)))

(defn my-interpose [z a-seq]
  (if (empty? a-seq)
    '()
    (my-interpose3 z (reverse a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                  (if (or (not (= nil e)) (= nil e))
                    (inc count)
                    count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce (fn [acc x] (conj acc x)) '() a-seq))

; alt
; (defn seq-min [a-seq]
;   (reduce min a-seq))
; alt
; (defn seq-max [a-seq]
;  (reduce max a-seq))
; alt
; (conj '[] (seq-min [2 15]) (seq-max [2 15]))

(defn min-max-element [a-seq]
  (reduce (fn [acc x]
            (cond
             (> (get acc 0) x) (assoc acc 0 x)
             (< (get acc 1) x) (assoc acc 1 x)
             :else acc))
          (conj '[] (first a-seq) (first a-seq)) (rest a-seq)))

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq) (conj '() n)
   (< n (first sorted-seq)) (concat (conj '() n) sorted-seq)
   :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce (fn [acc x]
            (insert acc x))
          (cons (first a-seq) '()) (rest a-seq)))

; from "looping is recursion"
(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    ))

(defn parity [a-seq]
  (reduce (fn [acc x]
            (toggle acc x))
          #{(first a-seq)} (rest a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (my-count more)))

(defn my-product [a-seq]
  (reduce (fn [acc x]
            (* acc x))
          (first a-seq) (rest a-seq)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (my-product (concat [x y] more))))

(defn pred-and
  ([] (fn [x] x))
  ([pred] (fn [x] (pred x)))
  ([pred-x pred-y] (fn [x] (and (pred-x x) (pred-y x))))
  ;                       (reduce max (max x y) more)
  ([pred-x pred-y & more] (reduce pred-and (pred-and pred-x pred-y) more)))

(defn my-map-one [f a-seq]
  (reduce (fn [acc x]
            (concat acc (conj '() (f x)))) '() a-seq))

(defn nth-of-all-re [n seqs]
  (reduce (fn [acc x]
    (concat acc (list (nth x n)))) '() seqs))

(defn my-map-helper-re [f seqs]
  (reduce (fn [acc x]
    (concat acc (conj '() (apply f (nth-of-all-re x seqs))))) '() (range 0 (count (first seqs)))))

(defn my-map
  ([f a-seq] (my-map-one f a-seq))
  ([f a-seq & more] (my-map-helper-re f (list* a-seq more))))
