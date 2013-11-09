(let [sum (Integer. (read-line))
      n   (Integer. (read-line))]
  (letfn
    [(nth-root
      [x n]
      (Math/pow Math/E (/ (Math/log x) n)))
     (sum-of-powers
      [sum n start]
      (let [upper-bound (int (inc (nth-root sum n)))]
        (->> (range start (inc upper-bound))
             (map
              (fn [x]
                (let [remain (- sum (int (Math/pow x n)))]
                  (cond (zero? remain) 1
                        (neg?  remain) 0
                        :else (sum-of-powers remain n (inc x))))))
             (reduce +))))]
    (println (sum-of-powers sum n 1))))
