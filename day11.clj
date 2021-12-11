(ns day11
  (:require [clojure.string :as str]))

(def test-sample
  "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

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
(defn ready? [i] (and (not (flashed? i)) (> i 8)))

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
  (if-not (seq (find-ready grid))
    grid
    (recur (step-2-once grid))))

(defn- reset [grid exhausted]
  (reduce (fn [grid pos] (assoc-in grid pos 0)) grid exhausted))

(defn- part1
  ([rounds grid] (part1 rounds grid 0))
  ([rounds grid flash-count]
   (if (zero? rounds)
     grid
     ;; flash-count
     (let [grid-next (->> grid
                          (step-1)
                          (step-2))
           exhausted (pos-for flashed? grid-next)]
       (part1 (dec rounds) (reset grid-next exhausted) (+ flash-count (count exhausted)))))))

(let [grid (->> test-sample
                (str/split-lines)
                (mapv (comp (partial mapv read-string) (partial re-seq #"\d"))))]
  (->> grid
       (part1 1)
       #_eof))
