(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (apply str (reduce (fn [s1 s2] (concat s1 " " s2)) a-seq ))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [s1 s2] (conj s1 x s2)) (vector (first a-seq)) (rest a-seq))))

(defn my-count [a-seq]
  (let [slength (fn [count _]
                  (inc count))]
    (reduce slength 0 a-seq)))



(defn my-reverse [a-seq]
  (let [flip (fn [f] (fn [& args] (apply f (reverse args))))]
  (reduce (flip cons) [] a-seq)))

(defn min-max-element [a-seq]
  (let [min-max (fn [[amin amax] elem]
        (cond 
          (< elem amin)
            (vector elem amax)
          (> elem amax)
            (vector amin elem) 
          :else 
            (vector amin amax)
          ))]
    (reduce min-max (vector (first a-seq) (first a-seq)) a-seq)))

(defn insert [sorted-seq n]
  (if (or (empty? sorted-seq) (< n (first sorted-seq)))
    (cons n sorted-seq)
    (cons (first sorted-seq) (insert (rest sorted-seq) n))))
    

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] 
    (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))]
  (reduce toggle #{} a-seq)))
  

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))
(defn count-params [& more] 
  (count more))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (fn [x] (reduce (fn [result mp] (and result (mp x))) (and (p1 x) (p2 x)) more))))

(defn firsts [a-seq] 
   (reduce (fn [result elem] (cons (first elem) result)) () a-seq))

(defn rests [a-seq]
   (reduce (fn [result elem] (cons (rest elem) result)) () a-seq))

(defn rotate-vectors [a-seq] 
    (loop [result []
           a-seq a-seq]
             (if (empty? (first a-seq))
               result
               (recur (conj result (firsts a-seq)) (rests a-seq)))))


(defn my-map [f & a-seq] 
  (let [my-map1 (fn [result elem] (conj result (apply f elem)))]
     (reduce my-map1 [] (rotate-vectors a-seq))))
 ; ([f a-seq & more] (let [my-map1 (fn [result elem] (conj result (apply f elem)))]
 ;     (reduce my-map1 (vector (apply f a-seq)) more))))
