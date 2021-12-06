(ns day6
  (:require [clojure.string :as str]))

(defprotocol FishLifeCycle
  (next-day [_])
  (kids-count [_]))

(defrecord Fish [cday kids]
  FishLifeCycle
  (next-day [it]
    (cond-> it
      (zero? cday) (-> (update :kids conj (->Fish 9 nil))
                       (assoc :cday 7))
      :else        (-> (update :cday dec)
                       (update :kids (partial map next-day)))))

  (kids-count [it]
    (reduce #(+ %1 (kids-count %2)) (count kids) kids)))

(defn- part1
  [input]
  (->> (reduce (fn [a _] (map #(next-day %) a)) input (range 80))
       (map kids-count)
       (apply +)
       (+ (count input))
       (println)))

(defn- pond-next
  [pond]
  (->> pond
       (map (fn [[day count]]
              (if (zero? day)
                {8 count 6 count}
                {(dec day) count})))
       (apply merge-with +)))

(let [starter
      (->> #_[3 4 3 1 2]
                   (str/split (slurp "input6.txt") #",")
                   (map #(-> % str/trim Integer/parseInt))
           )
      pond
      (reduce (fn [a day] (update a day (fnil inc 0))) (sorted-map) starter)]
  (->> (reduce (fn [p _] (pond-next p)) pond (range 256))
       (vals)
       (apply +)
       (println)
   #_(pond-next pond)))
