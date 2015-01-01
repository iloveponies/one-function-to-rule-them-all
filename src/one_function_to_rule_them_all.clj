(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (let [sep (fn [a x] (concat a x))]
    (reduce sep [] a-seq)))

(defn str-cat [a-seq]
  (let [catenate (fn [x s] (str x " " s))]
    (if (empty? a-seq)
      ""
      (if (= (count a-seq) 1)
        (first a-seq)
        (reduce catenate a-seq)))))
    
(defn my-interpose [x a-seq]
  (let [kjh (concat (vector (vector (first a-seq))) (rest a-seq))
        intp (fn [m s] (conj (conj m x) s))]
    (if (empty? a-seq)
      a-seq
      (if (= (count a-seq) 1)
        (vector (first a-seq))
        (reduce intp kjh))))) 

(defn my-count [a-seq]
  (let [cnt (fn [count a-seq] (inc count))]
    (reduce cnt 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev (fn [s e] (concat (vector e) s))]
    (reduce rev [] a-seq)))

(defn min-max-element [a-seq]
  (let [min-max (fn [r e] (cond 
                       (empty? r) (conj r e e)
                       (< e (first r)) (assoc r 0 e)
                       (> e (second r)) (assoc r 1 e)
                       :else r))]
    (reduce min-max [] a-seq)))

(defn insert [sorted-seq n]
  (let [bg (fn [a i] (reverse (nthrest (reverse a) (- (- (count a) 1) i))))]
  (loop [i 0]
        (if (empty? sorted-seq)
          (vector n)
          (if (> (first sorted-seq) n)
            (concat (vector n) sorted-seq)
            (if (apply < (concat (concat (bg sorted-seq i) (vector n)) (nthrest sorted-seq (+ i 1))))
              (concat (concat (bg sorted-seq i) (vector n)) (nthrest sorted-seq (+ i 1)))
         (recur (inc i))))))))

(defn insertion-sort [a-seq]
   (reduce insert [] a-seq))


(defn parity [a-seq]
  (let [apu (fn [e] 
              (loop [as a-seq
                     n 0]
                     (if (empty? as)
                       n
                       (if (= (first as) e)
                         (recur (rest as) (inc n))
                         (recur (rest as) n)))))
        check (fn [a e]
               (if (> (mod (apu e) 2) 0)
                 (set (concat a (vector e)))
                 (set a)))]
  (reduce check [] a-seq)))

(defn minus ([x] (- x))
            ([x y] (- x y)))

(defn count-params [& x]
  (let [count (fn [n a] (inc n))]
  (reduce count 0 x)))

(defn my-* ([] 1)
           ([x] x)
           ([x y] (* x y))
           ([x y & more] 
             (let [mult (fn [m e] (* m e))]
               (reduce mult (* y x) more))))

(defn pred-and [& x]
  (fn [e] (loop [p (first x)
                 r (rest x)]
                 (if (= p nil)
                   true
                   (if (not (p e))
                   false
                   (recur (first r) (rest r)))))))
    

(defn my-apu [& a]
  (let [ff (fn [c aa]
			  (loop [i 0
				     cb c
				     l (count c)
				     as aa]
				(if (or (= (first as) nil) (and (> l 0) (> i (- l 1))))
				  cb
				  (if (= l 0)
				    (recur (inc i) (conj cb (vector (first as))) l (rest as))
				    (recur (inc i) (assoc cb i (conj (nth cb i) (first as))) l (rest as))))))]
   (reduce ff [] (first a))))

(defn my-map [f & a] 
    (let [thi (fn [c ab]
               (if (= f vector)
                 (concat c (f ab))
                 (loop [cc c
                        aa ab
                        i 0]
                     (if (= nil (first aa))
                       cc
		               (if (= i 0)
		                 (recur (conj cc (f (first aa) (second aa))) (rest (rest aa)) (inc i))
		                 (recur (assoc cc (- (count cc) 1) (f (last cc) (first aa))) (rest aa) (inc i)))))))
           tha (fn [c ab] (conj c (f ab)))]
      (if (= (count a) 1)
        (reduce tha [] (first a))
        (reduce thi [] (my-apu a)))))              
                
         

    
    
    
    
