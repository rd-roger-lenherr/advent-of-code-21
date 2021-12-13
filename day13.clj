(ns day13
  (:require [clojure.string :as str]))

(defn- coord [s] (->> (str/split s #",") (mapv read-string)))
(defn- fold [s]
  (let [[_ axis v] (re-find #"(x|y)=(\d+)" s)]
    (if (= axis "x") [(read-string v) -1] [-1 (read-string v)])))

(defn- fold-fn [[on-x on-y]]
  (fn [[x y]]
    (cond
      (nil? (or x y)) nil
      (pos? on-y) (let [max-y (-> on-y (* 2) inc)]
                    (when (and (< y max-y) (not= y on-y))
                      (if (< y on-y)
                        [x y]
                        [x (- (dec max-y) y)])))
      (pos? on-x) (let [max-x (-> on-x (* 2) inc)]
                    (when (and (< x max-x) (not= x on-x))
                      (if (< x on-x)
                        [x y]
                        [(- (dec max-x) x) y]))))))

(defn- part1 [folds coords]
  (->> (map (fold-fn (first folds)) coords)
       (into #{})
       (count)))

(defn- display [coords]
  (let [[xr yr]
        (->> coords
             ((juxt (partial map first) (partial map second)))
             (map (comp range inc (partial apply max))))]
    (for [y yr
          x xr]
      (do (when (zero? x) (println))
          (doto (if (coords [x y]) "#" " ") print)))))

(defn- part2 [folds coords]
  (let [all-folds (->> (reverse folds) (map fold-fn) (apply comp))]
    (->> (map all-folds coords)
         (into #{}))))

(let [[scoords _ sfolds] (->> (slurp "input13.txt")
                              (str/split-lines)
                              (partition-by empty?))
      coords (map coord scoords)
      folds (map fold sfolds)]
  (println "Part 1:" (part1 folds coords))
  (display (part2 folds coords)))
