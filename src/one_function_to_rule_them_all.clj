(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  "Concatenates a sequence of sequences."
  (reduce concat a-seq))

(defn str-space [s1 s2]
  "Combines two strings with space in between."
  (str s1 " " s2))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce str-space a-seq)))

(defn my-interpose [x a-seq]
  "Interposes x between sequence elements."
  (if (empty? a-seq)
    []
    (let [f (fn [a b]
              (conj a x b))]
      (reduce f [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
    (reduce (fn [count x] (inc count))
            0
            a-seq))

(defn my-reverse [a-seq]
  "Reverses a sequence."
  (if (empty? a-seq)
    '()
    (let [f (fn [a b]
              (cons b a))]
      (reduce f [(first a-seq)] (rest a-seq)))))

(defn min-max-element [a-seq]
  "Returns the minimum and maximum of a sequence as a vector."
  (let [f (fn [[minimum maximum] elem]
                [(min minimum elem)
                 (max maximum elem)])]
      (reduce f [(first a-seq) (first a-seq)] ; Initial values for min and max is the first element
                a-seq)))

(defn insert [sorted-seq n]
  "Inserts n into a sorted list while keeping the list sorted."
  (loop [head []
         tail sorted-seq
         this (first sorted-seq)]
    (cond
     (empty? tail) (conj head n)                   ; Found the end, conj to the end
     (< n this)    (concat (conj head n) tail)     ; Is smaller than this element, insert between head and tail
     :else         (recur (conj head (first tail)) ; Loop: add first tail element to head ...
                          (rest tail)              ; ... drop one element from tail ...
                          (first (rest tail))))))  ; ... and update comparison element.

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  "Removes elem from a-set if a-set contains elem, and adds it to the set otherwise."
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  "Picks into a set those elements of a-seq that occur odd number of time."
  (reduce toggle #{} a-seq))

(defn minus
  "Is a minus."
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  "Counts the number of parameters given."
  ([& params] (my-count params)))

(defn my-*
  "Asterisk operator."
  ([& more] (reduce * 1 more)))

(defn pred-and-helper [pred1 pred2]
  "Returns a predicate that ANDs two predicates (given as parameters)."
  (fn [x] (and (pred1 x) (pred2 x))))

(defn pred-and
  ([] (fn [x] true))
  ([x] x)
  ([x & params]
   (reduce pred-and-helper x params)))

(defn firsts [a-seq]
  "Creates a list of firsts elements from a sequence of sequences."
  (reduce (fn [x elem]
            (conj x (first elem)))
          []
          a-seq))

(defn rests [a-seq]
  "Creates a list of tails from a sequence of sequences."
  (reduce (fn [x elem]
            (conj x (rest elem)))
            []
            a-seq))

(defn my-map
  "Map implementation using firsts and rests."
  ([f & seqs]
    (loop [l []
           heads (firsts seqs)
           tails (rests seqs)]
      (if (empty? (first tails))
       (conj l (apply f heads))
       (recur (conj l (apply f heads))
              (firsts tails)
              (rests tails))))))
