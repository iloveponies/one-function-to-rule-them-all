(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  "places x between every element of a-seq."
  (if (empty? a-seq)
    ()
    (reverse (butlast (reduce (fn [fst snd] (conj fst x snd)) () a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [acc e] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc e] (conj acc e)) () a-seq))

(defn min-max-element [a-seq]
  "returns the maximal and minimal elements
  of a-seq in a vector like [min max]"
  "(conj [] (apply min a-seq) (apply max a-seq)))"
  (into [] (concat (list (reduce (fn [acc e] (min acc e)) a-seq))
    (list (reduce (fn [acc e] (max acc e)) a-seq)))))

(defn insert [sorted-seq n]
  "Adds the number n into a sorted sequence of number.
   The ordering of the sequence must be preserved."
  (let [helpferf (fn [x] (< x n))]
    (if (empty? sorted-seq)
        (conj [] n)
        (concat
          (take-while helpferf sorted-seq)
          (list n)
          (drop-while helpferf sorted-seq)))))

(defn insertion-sort [a-seq]
  "Using reduce and the function insert."
  (reduce (fn [acc e] (insert acc e)) () a-seq))

(defn parity [a-seq]
  "Picks into a set those elements of a-seq that occur odd number of time."
  (let [toggle (fn [coll k] (if (contains? coll k) (disj coll k) (conj coll k)))]
    (reduce toggle #{} a-seq)))

(defn minus
  "Takes one or two parameters.
  If given a one parameter x, it returns −x,.
  If given two parameters x and y, it returns x−y."
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  "Accepts any number of parameters and
  returns how many it was called with. "
  ([& para] (count para)))

(defn my-*
  "Takes any number of parameters.
    If no parameters are given, return 1
    If one parameter x is given, return x.
    If two parameters x and y are given, return xy.
    If more than two parameters x, y, … are given, return their product x⋅y⋯.
  You are free to use *, but not apply."
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn pred-and
  "Write a new definition of pred-and that works for any amount of parameters.
    If no parameters are given, return a predicate that always returns true.
    If only one predicate p is given, return p.
    If two predicates are given, return a predicate that returns true if both of them return true and false otherwise.
    If more than two predicates are given,
      return a predicate that returns true only
      if all of the predicates return true and false otherwise."
  ([] (fn [x] true))
  ([p1] p1)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & pMore] (fn [x] (reduce (fn [acc e] (and acc (e x))) (and (p1 x) (p2 x)) pMore))))

(defn my-map
  "Works just like standard map. It takes one or more sequences
  and a function f that takes as many parameters as there are sequences"
  ([f col]
    (when (seq col)
      (lazy-seq
        (cons (f (first col))
            (my-map f (rest col))))))
  ([f c1 c2]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2)]
      (when (and s1 s2)
        (cons (f (first s1) (first s2))
              (map f (rest s1) (rest s2)))))))
  ([f c1 c2 c3]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2) s3 (seq c3)]
      (when (and  s1 s2 s3)
        (cons (f (first s1) (first s2) (first s3))
          (map f (rest s1) (rest s2) (rest s3)))))))
  ([f c1 c2 c3 & colls]
   (let [step (fn step [cs]
                 (lazy-seq
                  (let [ss (map seq cs)]
                    (when (every? identity ss)
                      (cons (map first ss) (step (map rest ss)))))))]
      (map #(apply f %) (step (conj colls c3 c2 c1))))))
