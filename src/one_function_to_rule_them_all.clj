(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(concat-elements [[1 2][3 4]])

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))


(defn my-interpose [x a-seq]
  (reduce (fn [y z] (conj (if (empty? y)  y (conj y x)) z)) []  a-seq)); pientä kikkailua jotta sekvenssin ensimmäinen ei olisi x, vaan ensimmäienn esiintyminen kun muodostettu sekvenssi on ei-tyhjä

(my-interpose "a" [])

(defn my-count [a-seq]
  (reduce (fn [count e] (inc count)) 0 a-seq))



(defn my-reverse [a-seq]
  (let [place-first (fn [s e] (cons e s))]
    (reduce place-first '() a-seq)))



(defn min-max-element [a-seq]
  (if (empty? a-seq)
    ()
    (let [paivita-arvot (fn [[min max] e] (if (< e min) [e max] (if (> e max) [min e] [min max])))]
      (reduce paivita-arvot [(first a-seq) (first a-seq)] a-seq))))



(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq)
       (cons n '())
    (<= (first sorted-seq) n)
       (cons (first sorted-seq) (insert (rest sorted-seq) n))
    :else
      (cons n sorted-seq)))





(defn insertion-sort [a-seq]
  (let [sijoita-alkuun (fn [alku e] (insert alku e))]
    (reduce sijoita-alkuun [] a-seq)))




(defn parity [a-seq]
  (let [toggle (fn [joukko e] (if (contains? joukko e) (disj joukko e) (conj joukko e)))]
    (reduce toggle #{} a-seq)))



(defn minus
  ([x] (- x))
  ([x y] (- x y)))



(defn count-params [& x]
  (let [lisaa-yhdella (fn [summa e] (inc summa))]
    (reduce lisaa-yhdella 0 x)))


(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & rest]
   (reduce (fn [tulo e] (my-* tulo e)) (my-* x y) rest)))



(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more]
   (reduce pred-and (pred-and p q) more)))





(defn my-map [f a-seq]
  [:-]) ;myöhemmäksi mietittäväksi...
