(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (cond
    (empty? a-seq) ""
    :else (reduce (fn [text word] (str text " " word)) a-seq)))

(defn my-interpose [x a-seq]
  (cond
   (empty? a-seq) '()
   :else (reduce (fn [elems elem] (conj elems x elem)) (vector (first a-seq)) (rest a-seq))))

(defn my-count [a-seq]
  (let [my-count-helper (fn [count elem] (inc count))]
    (reduce my-count-helper 0 a-seq)))

(defn my-reverse [a-seq]
  (let [my-reverse-helper (fn [elems elem] (cons elem elems))]
    (reduce my-reverse-helper [] a-seq )))

(defn min-max-element [a-seq]
  (let [fst (first a-seq)
        min-max-helper (fn [[min-elem max-elem] elem] (cond
                                                       (< elem min-elem) [elem max-elem]
                                                       (> elem max-elem) [min-elem elem]
                                                       :else [min-elem max-elem]))]
    (reduce min-max-helper [fst fst] a-seq)))

(defn insert [sorted-seq n]
 (let [insert-helper (fn [index a-seq] (cond
                                    (or (empty? a-seq) (> (first a-seq) n)) index
                                    :else (recur (inc index) (rest a-seq))))
       insert-at (insert-helper 0 sorted-seq)]
	(flatten (conj (drop insert-at sorted-seq) n (take insert-at sorted-seq)))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
    (loop [acc #{}
           a-seq a-seq]
      (cond
        (empty? a-seq) acc
        :else (recur (toggle acc (first a-seq)) (rest a-seq))))))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& a-seq]
  (count a-seq))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & a-seq] (reduce my-* (* x y) a-seq)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 & a-seq] (fn [x] (reduce (fn [acc elem] (and acc (elem x))) (p1 x) a-seq))))

(defn my-map [f & a-seq]
  (cond
    (some empty? a-seq) []
    :else (cons (apply f (map first a-seq)) (apply (partial my-map f) (map rest a-seq)))))