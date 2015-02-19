(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [memo item]
              (if (= 0 (count memo))
                (str memo item)        ; no leading space
                (str memo " " item)))  ; space before all other items
            ""
            a-seq)))

(defn my-interpose [x a-seq]
  (reverse (reduce (fn [memo item]
                     (if (= 0 (count memo))
                       (conj memo item)      ; no leading x
                       (conj memo x item)))  ; x before all other items
                   ()
                   a-seq)))

(defn my-count [a-seq]
  (let [counter 
        (fn [count e]
          (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [pred
        (fn [memo elem] 
          (concat [elem] memo))]
  (reduce pred () a-seq)))

(defn min-max-element [a-seq]
  (let [pred
        (fn [memo elem]
          (if (empty? memo)
            [elem elem]
            [(min (first memo) elem) (max (second memo) elem)]))]
  (reduce pred [] a-seq)))

(defn insert [sorted-seq n]
  (concat (take-while #(< % n) sorted-seq) 
        [n] 
        (drop-while #(< % n) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (set (map #(first %) (filter #(odd? (last %)) (frequencies a-seq)))))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-* [& x]
  (let [num-params (count x)]
    (cond
      (= num-params 0) 1
      (= num-params 1) x
      (= num-params 2) (* (first x) (second x))
      :else (reduce #(* %1 %2) 1 x))))

(defn pred-and [& x]
  (let [num-params (count x)]
    (cond 
      (= num-params 0) (fn [& y] true)
      (= num-params 1) (first x)
      (= num-params 2) (fn [y] 
                         (and ((first x) y) ((second x) y)))
      :else (reduce pred-and x))))

(defn my-map
  ([f] ())
  ([f coll] 
   (when-let  [s (seq coll)]
     (cons (f (first s)) (my-map f (rest s)))))
  ([f c1 c2]
   (let [s1 (seq c1) s2 (seq c2)]
     (when (and s1 s2)
       (cons (f (first s1) (first s2)) (my-map f (rest s1) (rest s2))))))
  ([f c1 c2 c3]
   (let [s1 (seq c1) s2 (seq c2) s3 (seq c3)]
     (when (and s1 s2 s3)
       (cons (f (first s1) (first s2) (first s3)) 
             (my-map f (rest s1) (rest s2) (rest s3)))))))



