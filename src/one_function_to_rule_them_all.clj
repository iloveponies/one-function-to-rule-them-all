(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 \space %2) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [a _](inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max [a b]
  [(min a b) (max a b)])

(defn min-max-compare [[min1 max1] a]
  [(min min1 a) (max max1 a)])

(defn min-max-element [[f & a-seq]]
  (if (empty? a-seq)
    [f f]
    (reduce min-max-compare
            [f f]
            a-seq)))

(defn insert [sorted-seq n]
  (let [[lesser greater](split-with #(< % n) sorted-seq)]
    (concat lesser (list n) greater)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity-helper [parity-set a]
  (if (parity-set a)
    (disj parity-set a)
    (conj parity-set a)))

(defn parity [a-seq]
  (reduce parity-helper #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & all] (apply * (conj all x y))))

(defn pred-and
  ([] (constantly true))
  ([x] (constantly x))
  ([x y] #(and (x %) (y %)))
  ([x y & more] (fn [k]
                  (reduce #(if % (%2 k)) true (list* x y more)))))

(defn at-least-one-empty? [& [f & r :as all-seqs]]
  (cond (empty? all-seqs) false
        (empty? f) true
        :else (recur r)))

(defn split-head-rest [& all-seqs]
  (letfn [(split-head-rest-iter [head-coll rest-coll [[f1 & r1] & r :as seqs]]
                                (if (empty? seqs)
                                  [head-coll rest-coll]
                                  (recur (conj head-coll f1)
                                         (conj rest-coll r1)
                                         r)))]
    (if (apply at-least-one-empty? all-seqs)
      nil
      (split-head-rest-iter () () all-seqs))))

; Construct the arguments as a vector
; Apply f over the vector

(defn apply-function [f args-seq]
  (reverse (reduce #(cons (apply f %2) %)
                   ()
                   args-seq)))

(defn derive-arguments [& all-seqs]
  (if-let [[heads rests] (apply split-head-rest all-seqs)]
    (cons heads (apply derive-arguments rests))))

(defn my-map [f & all-seq]
  (->> all-seq
       (apply derive-arguments)
       (apply-function f)))
