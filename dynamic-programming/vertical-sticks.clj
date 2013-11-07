(let [T (Integer. (read-line))]
  (dotimes [_ T]
    (read-line)
    (let [Y   (map #(Integer. %) (clojure.string/split (read-line) #"\s+"))
          n   (count Y)
          gap (fn [k] (/ (inc n) (inc k)))]
      (->> Y
           (map (fn [y] (gap (count (filter #(>= % y) Y)))))
           (reduce +)
           (double)
           (printf "%.2f\n")))))
