(let
  [F  (map #(Integer. %) (clojure.string/split (read-line) #"\s+"))
   P  (map #(Integer. %) (clojure.string/split (read-line) #"\s+"))
   [lo hi] (map #(Integer. %) (clojure.string/split (read-line) #"\s+"))
   f (fn [x] (->> P (map #(Math/pow x %)) (map * F) (reduce +)))
   r (->> (range lo (+ 0.001M hi) 0.001M) (map f))]
  (printf "%.1f\n" (* (->> r (reduce +)) 0.001M))
  (printf "%.1f\n" (->> r (map #(* (* Math/PI % %) 0.001M)) (reduce +))))
