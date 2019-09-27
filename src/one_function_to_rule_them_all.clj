(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [s1 s2] (str s1 " " s2)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce 
    (fn [accum e]
      (if (empty? accum)
        [e]
        (conj accum x e)))
    [] a-seq))

(defn my-count [a-seq]
  (let [count-fn (fn [count e]
                   (if (nil? e)
                     count
                     (inc count)))]
    (reduce count-fn 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev-fn (fn [s e]
                 (if (nil? e)
                   s
                   (conj s e)))]
    (reduce rev-fn `() a-seq)))

(defn min-max-element [a-seq]
  (let [min-max-fn (fn [min-max-vec e]
                     (if (empty? min-max-vec)
                       [e e]
                       (cond
                         (< e (first min-max-vec)) (assoc min-max-vec 0 e)
                         (> e (last min-max-vec)) (assoc min-max-vec 1 e)
                         :else min-max-vec)))]
    (reduce min-max-fn [] a-seq)))

(defn insert [sorted-seq n]
  (let [less-seq (filter (fn [e] (< e n)) sorted-seq)
        more-seq (filter (fn [e] (> e n)) sorted-seq)]
    (concat less-seq [n] more-seq)))

(defn insertion-sort [a-seq]
  (reduce (fn [sorted-seq n]
            (insert sorted-seq n)) [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    ))

(defn parity [a-seq]
  (let [parity-check-fn (fn [parity-set e]
                          (if (nil? e)
                            parity-set
                            (toggle parity-set e)))]
    (reduce parity-check-fn #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& more]
  (let [num-params (count more)]
    (cond
      (zero? num-params) 1
      (= num-params 1) (first more) ; return the param
      (= num-params 2) (* (first more) (second more)) ; multiply these 2 params
      :else (reduce * (first more) (rest more)))))

(defn pred-and [& preds]
  "Returns a function that evaluates the logical and of suplied preds."
     (fn [x]
       (reduce
         (fn [result pred]
           (if (pred x)
             (and result (pred x))
             (reduced false)))
         true
         preds)))

(defn get-firsts [& colls]
  (reduce (fn [coll elem]
             (conj coll (first elem)))
             []
             colls))

(defn get-rests [& colls]
  (reduce (fn [coll elem]
             (conj coll (rest elem)))
             []
             colls))

(defn my-map
  ([f coll] (loop [result `()
                   curr-coll coll]
              (if (empty? curr-coll)
                (reverse result)
                (recur (conj result (f (first curr-coll))) (rest curr-coll)))))
  ([f coll & colls] (loop [result `()
                           curr-colls (conj colls coll)]
                      (if (some empty? curr-colls)
                        (reverse result) ; If any are empty, kick out!
                        (recur  ; Otherwise, apply f again and recur.
                          (conj result (apply f (apply get-firsts curr-colls)))
                          (apply get-rests curr-colls)))))) 

