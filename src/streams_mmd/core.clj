(ns streams-mmd.core)

(defn ams-element [c t]
  (if (zero? (mod t c))
    c
    (mod t c)))

(defn ams-occurrences [c n t a]
  (let [diff  (- n t)
        m     (int (/ diff c))
        delta (if (<= a (mod diff c)) 1 0)]
    (+ m delta)))

(defn ams-random 
  [c n t]
  (let [a (ams-element c t)
        m (ams-occurrences c n t a)]
  (* n (dec (* 2 m)))))

(defn avg [s]
  (-> (reduce + s)
    (/ (count s))))

(let [f (partial map (partial ams-random 10 75))
      cases [[22 42 62]
             [25 34 47]
             [5 33 67]
             [4 31 72]]]
  (->> cases
    (map f) 
    (map avg) 
    (map (partial - 562))))
