(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (= exp 0)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (<= (count a-seq) 1)
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc seq1 seq2]
                 (cond 
                  ;; if seq is empty, return last value of acc
                  (and (empty? seq1) (empty? seq2)) acc
                  ;; if one of the sequence is empty or if acc is false
                  ;; return false and exit then and there
                  (or (empty? seq1) (empty? seq2) (= acc false)) false 
                  ;; otherwise compare first values of seq
                  ;; and call the function with rest of seq
                  :else
                  (recur (= (first seq1) 
                            (first seq2))
                         (rest seq1)
                         (rest seq2))))]
    (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  ;; define variables for recursion
  (loop [index -1
         found false
         seq a-seq]
    ;; recurse until either true is found or seq is empty 
    (cond
     (true? found) index
     (empty? seq) nil    ; return nil if element not found
     :else
      (recur (inc index) 
             (pred (first seq)) 
             (rest seq)))))

(defn avg [a-seq]
(if (empty? a-seq)                      ; skip loop if seq is empty
  0
  (loop [count 0
         sum 0
         seq a-seq]
    (if (empty? seq)
      (/ sum count)
      (recur (inc count) 
             (+ sum (first seq)) 
             (rest seq))))))

;;; only for sets. 
;;; Duplicate values will lead to error
(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

;;; returns set of elements occuring odd num of times  
(defn parity [a-seq]
  (loop [a-set #{}
         a-seq a-seq]
    (if (empty? a-seq)
      a-set
      (recur (toggle a-set (first a-seq)) 
             (rest a-seq)))))

;;; calculate fibonnaci number for given n  
(defn fast-fibo [n]
  (loop [count 1   ; because fib(2) loop should run only once 
         fib-prev 0
         fib-curr 1]
    (cond 
     (= n 0) fib-prev
     (= n 1) fib-curr
     (= count n) fib-curr ; limiting condition 
     :else 
     (recur
      (inc count)
      fib-curr  ; store in fib-prev     
      (+ fib-curr fib-prev))))) ; store in fib-curr 

;;; [1 2 1 3] => [1 2] 
;;; cut the sequence at repetition 
(defn cut-at-repetition [a-seq]
  (loop [list [] ; build a list to return
         set #{} ; build a set to check for duplicates
         seq a-seq]
    (cond 
     (empty? seq) list
     (contains? set (first seq)) list
     :else 
     (recur (conj list (first seq)) 
            (conj set (first seq))
            (rest seq)))))

