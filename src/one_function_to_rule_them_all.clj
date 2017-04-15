(ns one-function-to-rule-them-all)

(defn toggle [a-set elem]
   (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn concat-elements [a-seq]
   (reduce concat () a-seq))

(defn str-cat [a-seq]
   (if (empty? a-seq)
     ""
     (reduce (fn [prev fol] (str prev " " fol)) a-seq)))

(defn my-interpose [x a-seq]
   (if (empty? a-seq)
     '()
     (pop (reverse (reduce (fn [prev fol] (cons fol (cons x prev))) () a-seq)))))

(defn my-count [a-seq]
   (reduce (fn [acc elem] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
   (reduce (fn [prev fol] (into [fol] prev)) [] a-seq))

(defn min-max-element [a-seq]
   (if (empty? a-seq)
    nil
    (let [helper (fn [acc elem]
                   (cond
                    (empty? acc) [elem elem]
                    (< elem (acc 0)) [elem (acc 1)]
                    (> elem (acc 1)) [(acc 0) elem]
                    :else acc))]
      (reduce helper [] a-seq))))

(defn insert [sorted-seq n]
   (let [helper (fn [acc elem]
                    (if (and (< n elem) (not (contains? (set acc) n)))
                      (into acc (into [n] [elem]))
                      (into acc [elem])))
           inserted (reduce helper [] sorted-seq)]
       (if(== (count inserted) (count sorted-seq))
         (into inserted [n])
         inserted)))

(defn insertion-sort [a-seq]
   (reduce insert [] a-seq))

(defn parity [a-seq]
   (reduce (fn [acc elem] (toggle acc elem)) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([& more] (reduce (fn [acc elem] (inc acc)) 0 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 & more] (fn [x] (reduce (fn [acc p] (and acc (p x))) (p1 x) more))))


;kokoaa kaikkien listan vektorien ensimmäiset alkiot yhteen vektoriin
(defn reduce-seqs
  ([] '())
  ([& more]
   (loop [teh-vec []
          seq-cur (first more)
          seq-rest (rest more)]
     (cond
      (empty? seq-cur) []
      (empty? seq-rest) (into teh-vec [(first seq-cur)])
      :else (recur (into teh-vec [(first seq-cur)])
                   (first seq-rest)
                   (rest seq-rest))))))

;poistaa listan vektorien esimmäiset alkiot
(defn rest-of-reduce-seqs
  ([] '())
  ([& more]
   (loop [teh-vec []
          seq-cur (first more)
          seq-rest (rest more)]
     (cond
      (empty? seq-cur) []
      (empty? seq-rest) (into teh-vec [(rest seq-cur)])
      :else (recur (into teh-vec [(rest seq-cur)])
                   (first seq-rest)
                   (rest seq-rest))))))

;saadaan listan vektorit haluttuun muotoon
(defn reduce-whole-seqs
  ([] '())
  ([& more]
   (loop [teh-vec []
          seq-cur [(apply reduce-seqs more)]
          seq-rest (apply rest-of-reduce-seqs more)]
     (cond
      (empty? seq-cur) []
      (empty? (first seq-rest)) (into teh-vec seq-cur)
      :else (recur (into teh-vec seq-cur)
                   [(apply reduce-seqs seq-rest)]
                   (apply rest-of-reduce-seqs seq-rest))))))

(defn my-map
  ([f a-seq] (reduce (fn [acc elem] (into acc [(f elem)])) [] a-seq))
  ([f a-seq & more]
   (let [whole-set (concat [a-seq] more)
         reduced-set (apply reduce-whole-seqs whole-set)]
     (reduce (fn [acc elem] (into acc [(apply f elem)])) [] reduced-set))))





