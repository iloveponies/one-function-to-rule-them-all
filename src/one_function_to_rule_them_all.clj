(ns one-function-to-rule-them-all)

(defn concat-elements
  ([a-seq]
   (let [concat-helper (fn [acc elem]
                         (if (nil? elem)
                           acc
                           (concat acc elem)))]
     (reduce concat-helper [] a-seq))))

(defn str-cat [a-seq]
  (let [str-cat-helper (fn [acc elem]
                         (if (nil? elem)
                           acc
                           (str acc " " elem)))]
    (if (empty? a-seq)
      ""
      (reduce str-cat-helper (first a-seq) (rest a-seq)))))

(defn my-interpose [x a-seq]
  (let [my-interpose (fn [acc elem]
                         (if (nil? elem)
                           acc
                           (conj acc x elem)))]
    (if (or (empty? a-seq)
            (< (count a-seq) 2))
      a-seq
      (seq (reduce my-interpose (conj [] (first a-seq)) (rest a-seq))))))

(defn my-count [a-seq]
  (let [my-count (fn [n elem]
                   (if (nil? elem)
                     n
                     (inc n)))]
    (reduce my-count 0 a-seq))) 

(defn my-reverse [a-seq]
  (let [my-reverse (fn [acc elem]
                         (if (nil? elem)
                           acc
                           (conj acc elem)))]
    (reduce my-reverse '() a-seq)))

(defn min-max-element [a-seq]
  (let [min-max-element (fn [min-max  next-elem]
                          (if (nil? next-elem)
                            min-max
                            (cond
                              (< next-elem (first min-max)) [next-elem (last min-max)] 
                              (> next-elem  (last min-max)) [(first  min-max) next-elem]
                              :else min-max)))]
    (reduce min-max-element [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (let [insert (fn [init sorted-seq n]
                 (if (empty? sorted-seq)
                   (conj init n)
                   (if (< n (first sorted-seq))
                     (concat (conj init n) sorted-seq)
                     (recur (conj init (first sorted-seq)) (rest sorted-seq) n))))]
    (insert [] sorted-seq n)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq)) 

(defn parity [a-seq]
  (let [parity (fn [init next-elem]
                 (if (nil? next-elem)
                   init
                   (if (contains? init next-elem)
                     (disj init next-elem)
                     (conj init next-elem))))]
    (reduce parity #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x & more] (+ 1 (count more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [_] true))
  ([p1] (fn [x] (p1 x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map
  ([f & more]
   (let [transpose (apply mapv vector more) 
         mf (fn [acc next-elem]
              (if (empty? next-elem)
                acc
                (conj acc (apply f next-elem))))]
     (seq (reduce mf [] transpose))))) 
