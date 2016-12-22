(ns one-function-to-rule-them-all)

; (concat-elements [[1 2] [3 4]]) ;=> (1 2 3 4)
(defn concat-elements [a-seq]
  (reduce concat () a-seq))

; (str-cat ["I" "am" "Legend"])  ;=> "I am Legend"
(defn str-cat [a-seq] 
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

; (my-interpose "," ["I" "me" "myself"]) ;=> ("I" "," "me" "," "myself")
(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (let [ counter (fn [count e]
                   (inc count)) ]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [ helper (fn [reversed e]
                  (cons e reversed)) ]
    (reduce helper [] a-seq)))

(defn min-max-element [a-seq]
  (let [ helper (fn [min-max e]
                  (if (empty? min-max) 
                    [e e]
                    (do
                      (cond 
                        (< e (first min-max)) [e (last min-max)]
                        (> e (last min-max)) [(first min-max) e]
                        :else min-max)))) ]
    (reduce helper [] a-seq)))
                    
(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (conj sorted-seq n)
    (do 
      (loop [counter 0
             from-seq sorted-seq
             to-seq [] ]
        (if (and (not (empty? from-seq)) (> n (first from-seq)))
          (recur (inc counter) (rest from-seq) (conj to-seq (first from-seq)))
          (concat (conj to-seq n) from-seq))))))

(defn insertion-sort [a-seq]
  (if (empty? a-seq)
    a-seq
    (do 
      (reduce insert [] a-seq))))

(defn parity-2 [a-seq]
  (if (empty? a-seq)
    #{}
    (do 
      (set 
       (map first 
            (filter #(odd? (second %))
                    (reduce (fn [counts elem] 
                              (if (contains? counts elem)
                                (assoc counts elem (inc (get counts elem)))
                                (assoc counts elem 1))) {} a-seq)))))))

(defn parity [a-seq]
  (if (empty? a-seq)
    #{}
    (do 
      (->> a-seq
       (reduce (fn [counts elem]
                 (if (contains? counts elem)
                   (assoc counts elem (inc (get counts elem)))
                   (assoc counts elem 1))) {} )
       (filter #(odd? (second %)))
       (map first)
       (set)))))


(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))


(defn count-params [ & more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)  
  ([x y] (* x y))
  ([x y & more]
   (let [vals (-> more
                  (conj x)
                  (conj y)) ]
   (reduce * vals))))

(defn pred-and
  ([] (fn [x] true))
  ([pred] (fn [x] (pred x)))
  ([pred1 pred2] (fn [x] (if (and (pred1 x) (pred2 x))
                           true
                           false )))
  ([pred1 pred2 & more] (fn [x] (let [all_preds (-> more
                                                    (conj pred1)
                                                    (conj pred2)) ]
                                (empty? (filter false? (map #( % x) all_preds)))))))


(defn my-map 
  ([f a-seq]
   (if (empty? a-seq)
     a-seq
     (cons (f (first a-seq)) (my-map f (rest a-seq)))))
  ([f a-seq & colls]
   (let [step (fn step [cs]
                 (lazy-seq
                  (let [ss (map seq cs)]
                    (when (every? identity ss)
                      (cons (map first ss) (step (map rest ss)))))))]
     (map #(apply f %) (step (conj colls a-seq))))))


; (my-map inc [1 2 3 4])                  ;=> (2 3 4 5)
; (my-map + [1 1 1] [1 1 1] [1 1 1])      ;=> (3 3 3)
; (my-map vector [1 2 3] [1 2 3] [1 2 3]) ;=> ((1 1 1) (2 2 2) (3 3 3))
