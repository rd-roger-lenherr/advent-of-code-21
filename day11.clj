(ns day11
  (:require [clojure.string :as str]))

(defn- step-1 [grid]
  (let [row-size (count (first grid))]
    (->> (flatten grid)
         (map inc)
         (partition row-size)
         (mapv vec))))

(defn- flash-pos [grid [y x]]
  (->> (for [y (range (max 0 (dec y)) (inc (min (dec (count grid)) (inc y))))
             x (range (max 0 (dec x)) (inc (min (dec (count (first grid))) (inc x))))]
         [y x])
       (remove #{[y x]})))

(def flashed 100)
(def flashed? (partial < (dec flashed)))
(defn ready? [i] (and (not (flashed? i)) (> i 9)))

(defn- pos-for [f grid]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :let [v (get-in grid [y x])]
        :when (f v)]
    [y x]))

(defn- step-2-once [grid]
  (reduce (fn [grid pos]
            (-> (reduce (fn [grid fpos]
                          (update-in grid fpos inc))
                        grid (flash-pos grid pos))
                (assoc-in pos flashed)))
          grid (pos-for ready? grid)))

(defn- step-2 [grid]
  (if-not (seq (pos-for ready? grid))
    grid
    (recur (step-2-once grid))))

(defn- reset [grid exhausted]
  (reduce (fn [grid pos] (assoc-in grid pos 0)) grid exhausted))

(defn- part1
  ([rounds grid] (part1 rounds grid 0))
  ([rounds grid flash-count]
   (if (zero? rounds)
     flash-count
     (let [grid-next (->> grid
                          (step-1)
                          (step-2))
           exhausted (pos-for flashed? grid-next)]
       (part1 (dec rounds) (reset grid-next exhausted) (+ flash-count (count exhausted)))))))

(defn- part2
  ([grid] (part2 grid 0))
  ([grid round]
   (if (every? zero? (flatten grid))
     round
     (let [grid-next (->> grid step-1 step-2)
           exhausted (pos-for flashed? grid-next)]
       (recur (reset grid-next exhausted) (inc round))))))

(let [grid (->> (slurp "input11.txt")
                (str/split-lines)
                (mapv (comp (partial mapv read-string) (partial re-seq #"\d"))))]
  (println (part1 100 grid))
  (println (part2 grid)))
