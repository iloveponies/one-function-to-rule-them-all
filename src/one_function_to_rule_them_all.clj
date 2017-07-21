(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
 (let [helper (fn [f r]
               (str f " " r))]
  (if (empty? a-seq)
   ""
   (reduce helper a-seq))))
  

(defn my-interpose [x a-seq]
  (let [helper (fn [f r]
                (conj f x r))]
       (if (empty? a-seq)
        []
        (drop 1 (reduce helper [] a-seq)))))
        
       

(defn my-count [a-seq]
  (let [counter (fn [count e]
                 (inc count))]
       (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [helper (fn [f r]
                (cons r f))]
       (reduce helper [] a-seq)))

(defn min-max-element [a-seq]
 (reduce (fn [[mi ma] e]
          [(min mi e) (max ma e)])
         [(first a-seq) (first a-seq)]
         a-seq))

  
  

(defn insert [sorted-seq n]
 (loop [b []
        r sorted-seq]
       (cond
        (empty? r) (conj b n)
        (<= n (first r)) (into (conj b n (first r)) (rest r))
        :else (recur (conj b (first r)) (rest r)))))
       
        

  

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))


(defn toggle [a-set elem]
  (if (contains? a-set elem) 
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus 
 ([x] (- x))
 ([x y] (- x y)))
  

(defn count-params 
 ([] 0)
 ([x & more]
  (let [helper (fn [c e]
                (inc c))]
   (reduce helper (helper 0 x) more))))
  

(defn my-* 
 ([] 1)
 ([x] x)
 ([x y] (* x y))
 ([x y & more] (reduce my-*(my-* x y) more)))

(defn pred-and 
 ([] (fn [x] true))
 ([pred1](fn [x] (pred1 x)))
 ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
 ([pred1 pred2 & more] (reduce pred-and (pred-and pred1 pred2) more)))

(defn my-map 
 ([f a-seq] (reduce (fn [new e]
                     (conj new (f e)))
                    []
                    a-seq))
 ([f a-seq & more]
  (let [trans (apply mapv vector (into [] (concat [a-seq] more)))]
   (my-map #(apply f %) trans))))

  
                     
  
