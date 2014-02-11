(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce (fn [acc elem ] (str acc " " elem)) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [acc elem] (concat acc (conj (vector x) elem))) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc elem] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc elem] (concat (vector elem) acc)) [] a-seq))

(defn min-max-element [a-seq]
  (let [comp-max-min (fn [[curr-min curr-max] elem] (cond
                                                      (and (nil? curr-max) (nil? curr-min)) [elem elem]
                                                      (<= elem curr-min) [elem curr-max]
                                                      (>= elem curr-max) [curr-min elem]
                                                      :else [curr-min curr-max]))]
    (reduce comp-max-min [] a-seq)))

(defn insert [sorted-seq n]
  (let [helper-func (fn [result v] (cond 
                                      (empty? v) (concat result (vector n))
                                      (>= n (first v)) (recur (conj result (first v)) (rest v)) 
                                      :else (concat (conj result n) v)))]
    (helper-func [] sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus 
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x & more] (reduce (fn [acc elem] (+ acc 1)) 1 more )))
  ;([] 0)
  ;([x] 1)
  ;([x y] 2)
  ;([x y & more] (

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

;(defn pred-and [pred1 pred2]
;  (fn [x] (if (and (pred1 x) (pred2 x)) true false)))

;(defn pred-and
;  ([] (fn [x] (if (= x x) true false)))
;  ([pred] (fn [x] (pred x)))
;  ([pred1 pred2] (fn [x] (if (and (pred1 x) (pred2 x)) true false)))
;  ([pred1 pred2 & more] (fn [x] (if ((pred-and pred1 pred2) x) (reduce (fn [pred elem] ()) false ))))
;  ([pred1 pred2 & more] (let [helper (fn [x pred] (if (pred1 x) ((if (pred2 x) (recur (first more) false)) false)  

;(defn pred-and
;  ([] (fn [x] (if (= x x) true false)))
;  ([pred] (fn [x] (pred x)))
;  ([pred & more] (fn [x] (reduce (fn [acc elem] (if (and acc (elem x)) true false)) true more))))

(defn preds-helper
    [x preds]
        (cond 
          (empty? preds) true
          ((first preds) x) (preds-helper x (rest preds))
           :else false))

(defn pred-and
  ([] (fn [x] (if (= x x) true false)))
  ([pred] (fn [x] (pred x)))
  ([pred & more] (fn [x] (if (pred x) (preds-helper x more) false))))

;  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
;  ([pred1 pred2 & more] (fn [x] (reduce (fn [acc new-pred] (and (new-pred x) acc)) true more)))) 

(defn my-map [f a-seq]
  [:-])