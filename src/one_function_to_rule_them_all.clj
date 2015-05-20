(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [str1 str2] (str (str str1 " ") str2)) a-seq)
  ))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)  
    []
    (reduce (fn [jono alkio2] 
        (conj (conj jono x) alkio2)) 
      [(first a-seq)]
      (rest a-seq)
    )))

(defn my-count [a-seq]
  (reduce (fn [laskuri alkio] (inc laskuri)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [tulosjono alkio] (cons alkio tulosjono)) [] a-seq
  ))

(defn min-max-element [a-seq]
  (reduce 
    (fn [tulos alkio] 
      (assoc (assoc tulos 0 (min alkio (first tulos))) 1 (max alkio (second tulos))))
    [(first a-seq) (first a-seq)]
    a-seq
  ))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
    (loop [seq sorted-seq luku n mones 0]
      (if (< (dec (count seq)) mones)
        (conj seq luku)
        (if (> (get seq mones) luku)
          (recur (assoc seq mones luku) (get seq mones) (inc mones))
          (recur seq luku (inc mones))
        ))
    )))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce 
    (fn [tulos alkio] 
      (if (contains? tulos alkio)
        (disj tulos alkio)
        (conj tulos alkio))
    )
    #{}
    a-seq
  ))

(defn minus 
  ([x] (- x))
  ([x y](- x y))
)

(defn count-params [& kaikki]
  (reduce (fn [x alkio] (inc x)) 0 kaikki)
)

(defn my-* [& kaikki]
  (cond
    (= (count kaikki) 0) 1
    :else (reduce 
      (fn [tulos alkio] (* tulos alkio))
      1
      kaikki
    )))

(defn pred-and [& kaikki]
  (cond 
    (< (count kaikki) 2) (fn [x] true)
    :else  (fn [x] (reduce (fn [tulos alkio] (and tulos (alkio x))) true kaikki))
  ))

(defn samankohtaiset-alkiot [jonojonoja]
  "Palauttaa jonon vektoreita, joista n:nnessä on jonojen n:nnet alkiot"
  (apply vector (cond
      (empty? (first jonojonoja)) '()
      :else (conj (samankohtaiset-alkiot (map rest jonojonoja)) (apply vector (map first jonojonoja))
      ))))

(defn my-map [f & jonot]
  (reverse (apply vector (map
    (fn [alkiovektori] (apply f alkiovektori))
    (samankohtaiset-alkiot jonot)
  ))))
