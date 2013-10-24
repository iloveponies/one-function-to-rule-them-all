(ns one-function-to-rule-them-all)

; OK
(defn concat-elements [a-seq]
  "Concatenates a sequence of sequences."
  (reduce concat a-seq))

; OK
(defn str-space [s1 s2]
  "Combines two strings with space in between."
  (str s1 " " s2))

; OK
(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce str-space a-seq)))

; OK
(defn my-interpose [x a-seq]
  "Interposes x between sequence elements."
  (if (empty? a-seq)
    []
    (let [f (fn [a b]
              (conj a x b))]
      (reduce f [(first a-seq)] (rest a-seq)))))

; OK
(defn my-count [a-seq]
    (reduce (fn [count x] (inc count))
            0
            a-seq))

; OK
(defn my-reverse [a-seq]
  "Reverses a sequence."
  (if (empty? a-seq)
    '()
    (let [f (fn [a b]
              (cons b a))]
      (reduce f [(first a-seq)] (rest a-seq)))))

(defn min-max-element [a-seq]
  ":)")

(defn insert [sorted-seq n]
  ":/")

(defn insertion-sort [a-seq]
  ":%")

(defn parity [a-seq]
  ":4")

; OK
(defn minus
  "Is a minus."
  ([x] (- x))
  ([x y] (- x y)))

; OK
(defn count-params
  ([] 0)
  ([x] 1)
  ([x & more] (+ 1(count more))))

(defn my-* [x]
  ":)")

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  "://")
