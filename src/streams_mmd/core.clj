(ns streams-mmd.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3: Moments - Surprise Number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ams-element [c t]
  (if (zero? (mod t c))
    c
    (mod t c)))

(defn ams-occurrences [c n t a]
  (let [diff  (- n t)
        m     (int (/ diff c))]
    (inc m)))

(defn ams-random 
  [c n t]
  (let [a (ams-element c t)
        m (ams-occurrences c n t a)]
  (* n (dec (* 2 m)))))

(defn avg [s]
  (-> (reduce + s)
    (/ (count s))))

(let [f (partial map (partial ams-random 10 75))
      cases [[3 45 72]
             [9 50 68]
             [14 35 42]
             [25 34 47]]]
  #_(prn (map f cases))
  (->> cases
    (map f) 
    (map avg) 
    (map (partial - 565))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 4: Counting Distinct Elements - Flagolet-Martin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn hash-fn [x]
  (mod (+ 7 (* 3 x)) 11))

(defn trailing-zeros [x]
  (loop [n 0
         x x]
    (cond
      (zero? x) n
      (zero? (bit-and x 1))
        (recur (inc n) (bit-shift-right x 1))
      :else
        n)))

(let [f (partial map (comp trailing-zeros hash-fn))
      cases [[4 5 6 10]
             [2 6 8 10]
             [2 4 6 10]
             [2 6 8 9]]]
  (->>
    (map (juxt identity f) cases)
    (filter (fn [[x y]] (= (apply max y) 2)))
    ffirst))



