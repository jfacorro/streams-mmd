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
      cases [[17, 43, 51]
[31, 48, 50]
[25, 34, 47]
[37, 46, 55]
]
      #_[[25 34 47]
             [31 32 44]
             [20 49 53]
             [24 44 65]]]
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
      cases [[4, 6, 9, 10]
             [2, 5, 7, 10]
             [ 3, 7, 8, 10]
             [1, 2, 3, 9]] 
      #_[[4 5 6 7]
             [2 5 7 10]
             [4 5 6 10]
             [1 3 9 10]]
      #_[[4 5 6 10]
         [2 6 8 10]
         [2 4 6 10]
         [2 6 8 9]]]
  (->>
    (map (juxt identity f) cases)
    (filter (fn [[x y]] (= (apply max y) 2)))))



