(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq)
)

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq))
)

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq))
)

(defn my-count [a-seq]
  (let [counter (fn [count counter]
                    (inc count)
                )
       ]
    (reduce counter 0 a-seq)
  )
)

(defn my-reverse [a-seq]
  (reduce conj '() a-seq)
)

(defn min-max-element [a-seq]
   (reduce conj (vector (reduce min a-seq)) (vector (reduce max a-seq)))
)


(defn goneSeq [inseq uptil]
  (loop [counter 0  newSeq [] orginSeq inseq]
        (if (== counter uptil)
                newSeq
                (recur (inc counter) (conj newSeq (first orginSeq)) (rest orginSeq))
          )
      
    )
  )
(defn insert [sorted-seq n]
  (loop [currentSeq sorted-seq counter 0]
     (cond (empty? currentSeq) (concat sorted-seq (vector n))
         (<= n (first currentSeq)) (concat (goneSeq sorted-seq counter) (vector n) currentSeq)
           :else (recur (rest currentSeq) (inc counter))
     )
  )
  )

(defn insertion-sort [a-seq]
    (reduce insert [] a-seq)   
)

(defn parity [a-seq]
  (let [freqs (frequencies a-seq)]
      (reduce (fn [s [k v]] 
                (if (odd? v)
                  (conj s k) 
                  s
                )
              ) 
      #{} freqs
      )
  )
)

(defn minus ([x] (- 0 x))
   ([x y] (- x y))
)

(defn count-params[& args]
  (count args)
  )

(defn my-*
  ([]  1)
  ([x] x)                         ; one parameter
  ([x y] (* x y))                 ; two parameters
  ([x y & more]                   ; more than two parameters
    
 
    (reduce * (my-* x y) more))
  )

(defn andp [& fns]
  (fn [& args]
    (every? #(apply % args) fns)))

(defn pred-and
      ([] (fn [x] true))
      ([x] x)
      ([x y] (andp x y))
      ([x y & more]
          (reduce pred-and (pred-and x y) more)
        ) 
)

(defn my-map ([f a-seq]
     (reduce f a-seq)
  )
  ([f seq1 seq2]
    (let [s1 (seq seq1) s2 (seq seq2)]
         (when (and s1 s2)
           (cons (f (first s1) (first s2))
                 (map f (rest s1) (rest s2))
                 )
    )))
   ([f seq1 seq2 & more]
          (reduce my-map (my-map f seq1 seq2) more)
    )

)