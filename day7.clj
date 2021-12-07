(ns day7)

(defn- consumption-flat
  [pos xs]
  (->> (map #(-> (- pos %) Math/abs) xs)
       (reduce +)))

(defn- consumption-rolling
  [pos xs]
  (->> (map #(-> (- pos %) Math/abs) xs)
       (map #(-> (inc %) (* %) (/ 2)))
       (reduce +)))

(defn- sweet-spot
  [fuel-cost-fn sample]
  (let [low (apply min sample)
        high (apply max sample)]
    (->> (range low (inc high))
         (reduce (fn [[_ fuel-best :as best] pos]
                   (let [fuel (fuel-cost-fn pos sample)]
                     (if (< fuel fuel-best) [pos fuel] best)))
                 [-1 Long/MAX_VALUE]))))

(let [sample (as-> (slurp "input7.txt") _
               (clojure.string/split _ #",")
               (map read-string _))]
  (println "Part 1 [pos fuel]: " (sweet-spot consumption-flat sample))
  (println "Part 2 [pos fuel]: " (sweet-spot consumption-rolling sample)))
