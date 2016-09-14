(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* base acc) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc a-seq]
                 (if (empty? a-seq)
                   acc
                   (recur (first a-seq) (rest a-seq))))]
    (helper (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (and (= (count seq1) (count seq2)) (= (first seq1) (first seq2))) (recur (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [n 0
         curr-seq a-seq]
    (if (empty? curr-seq)
      nil
      (if (pred (first curr-seq))
        n
        (recur (inc n) (rest curr-seq))))))

(defn avg [a-seq]
  (loop [sum 0
         curr-seq a-seq]
    (if (empty? curr-seq)
      (/ sum (count a-seq))
      (recur (+ sum (first curr-seq)) (rest curr-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [curr-seq a-seq
         acc #{}]
    (if (empty? curr-seq)
      acc
      (recur (rest curr-seq) (toggle acc (first curr-seq))))))

(defn fast-fibo [n]
  (loop [i 0
         nth 0
         nth-1 1]
    (cond (= 0 n) 0
          (= 1 n) 1
          (= i n) nth
          :else   (recur (inc i) (+ nth nth-1) nth))))

(defn cut-at-repetition [a-seq]
  (loop [test-acc #{}
         acc []
         curr-seq a-seq]
    (let [candidate-acc (conj test-acc (first curr-seq))]
      (if (or (empty? curr-seq) (= (count acc) (count candidate-acc)))
        acc
        (recur candidate-acc (conj acc (first curr-seq)) (rest curr-seq))))))

