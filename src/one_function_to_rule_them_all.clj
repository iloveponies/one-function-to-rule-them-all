(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (cond
    (empty? a-seq) ""
    :else (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (drop 1 (reduce (fn [e1 e2] (conj e1 x e2)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [count _] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [x y] (conj x y)) '() a-seq))

(defn min-max-element [a-seq]
  (let [helper (fn [pair,e] (if (empty? pair)
                              [e,e]
                              [(min e (first pair)),(max e (second pair))]))]
  (reduce helper [] a-seq)))

(defn insert [sorted-seq n]
  (loop [pre-seq [] 
         post-seq sorted-seq]

    (let [f-elem (first post-seq)]
      (cond
        (or (empty? post-seq) (< n f-elem)) (concat pre-seq (cons n post-seq))
        :else (recur (conj pre-seq f-elem) (rest post-seq))))))
    

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                  (if (contains? a-set elem)
                    (disj a-set elem)
                    (conj a-set elem)))]

    (reduce toggle '#{} a-seq)))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (reduce (fn [x _] (inc x)) 0 params))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & params] (reduce * (* x y) params)))

(defn pred-and
  ([] (fn [_] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & preds] (reduce pred-and (pred-and p1 p2) preds)))  

(defn my-map [f & seqs]
  (apply map f seqs))
  






