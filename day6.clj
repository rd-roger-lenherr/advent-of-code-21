(ns day6)

(defn- pond-next
  [pond]
  (->> pond
       (map (fn [[day count]]
              (if (zero? day)
                {8 count 6 count}
                {(dec day) count})))
       (apply merge-with +)))

(defn- fish-total
  [pond days]
  (->> (reduce (fn [p _] (pond-next p)) pond (range days))
       (vals)
       (apply +)))

(let [input (->> (clojure.string/split (slurp "input6.txt") #",")
                 (map #(-> % clojure.string/trim Integer/parseInt)))
      ;; TODO: could have used `frequencies` instead of the reduce <3
      pond (reduce (fn [pond day] (update pond day (fnil inc 0))) (sorted-map) input)]
  (println "Part1: " (fish-total pond 80))
  (println "Part2: " (fish-total pond 256)))
