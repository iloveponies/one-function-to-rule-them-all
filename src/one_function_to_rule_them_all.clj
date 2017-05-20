(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  ;; or just `(clojure.string/join " " a-seq)`
  (if (empty? a-seq)
    ""
    (let [cat (fn [sum new]
                (str sum " " new))]
      (reduce cat a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (let [interposer (fn [sum elem]
                       (conj sum x elem))]
      ;; Could someone please merge some PRs? Instructions say `'()` but tests
      ;; expect `[]`..........................................................
      (reduce interposer [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [c elem]
                  (inc c))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverse-adder (fn [sum elem]
                        (conj sum elem))]
    (reduce reverse-adder '() a-seq)))

(defn min-max-element [a-seq]
  (let [min-max-counter (fn [{maximum :max
                              minimum :min} elem]
                          {:max (max maximum elem) :min (min minimum elem)})]
    ;; Transform the map to an array
    (let [{:keys [min max]}
          (reduce min-max-counter {:min (Integer/MAX_VALUE) :max (Integer/MIN_VALUE)} a-seq)]
      [min max])))

(defn insert [sorted-seq n]
  (loop [new-seq '()
         sorted-seq sorted-seq]
    (let [f (first sorted-seq)]
      (cond (empty? sorted-seq) (concat new-seq [n])
            (<= n f) (concat new-seq [n f] (rest sorted-seq))
            :else (recur (concat new-seq [f]) (rest sorted-seq))))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [paritier (fn [{:keys [evens odds]} n]
                   (cond
                     (some #{n} odds) {:evens (conj evens n)
                                       :odds (disj odds n)}
                     (some #{n} evens) {:evens (disj evens n)
                                        :odds (conj odds n)}
                     :else {:evens evens
                            :odds (conj odds n)}))]
    (:odds (reduce paritier {:evens #{}
                             :odds #{}} a-seq))))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([& x] (reduce * x)))

(defn pred-and
  ([] (fn [_] true))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn firsts [seq-of-seqs]
  (let [get-first (fn [firsts-seq a-seq]
                    (concat firsts-seq [(first a-seq)]))]
    (reduce get-first '() seq-of-seqs)))

(defn rests [seq-of-seqs]
  (let [get-rest (fn [rests-seq a-seq]
                   (concat rests-seq [(rest a-seq)]))]
    (reduce get-rest '() seq-of-seqs)))

(defn my-map
  ([f & more]
   (loop [new-seq '()
          seq-of-seqs more]
     (if (some empty? seq-of-seqs) new-seq
         (recur (concat new-seq [(apply f (firsts seq-of-seqs))]) (rests seq-of-seqs))))))
