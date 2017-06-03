(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (= 1 (count coll)))

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq)
                   (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max (first a-seq)
                       (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (cons (first a-seq)
                                    (my-filter pred? (rest a-seq)))
        :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (== elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (not (pred? (first a-seq))) '()
        :else (cons (first a-seq)
                    (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (not (pred? (first a-seq))) a-seq
        :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (not= (count a-seq) (count b-seq)) false
        (not= (first a-seq) (first b-seq)) false
        :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        (> n 1) (+ (fib (- n 1))
                   (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (cons (apply list a-seq)
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (cons (apply list a-seq)
          (inits (take (dec (count a-seq)) a-seq)))))

(defn rotations [a-seq]
  (concat (rest a-seq)
        (list (first a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          amount (get freqs elem)
          new-freqs (if (nil? amount)
                      (assoc freqs elem 1)
                      (update-in freqs [elem] inc))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [seqs a-map]
  (if (empty? a-map)
    seqs
    (let [a-key (key (first a-map))
          amount (get a-map a-key)
          new-seqs (concat seqs (repeat amount a-key))]
      (un-frequencies-helper new-seqs (dissoc a-map a-key)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper '() a-map))

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (if (empty? a-seq)
    []
    (let [half (int (/ (count a-seq) 2))]
      (vector (take half a-seq) (drop half a-seq)))))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (let [next-a (first a-seq)
          new-b (concat (filter (fn [b] (<= b next-a)) b-seq)
                        [next-a]
                        (filter (fn [b] (> b next-a)) b-seq))]
      (seq-merge (rest a-seq) new-b))))

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

