(ns day13
  (:require [clojure.string :as str]))

(def test-sample
  "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

(defn- coord [s] (->> (str/split s #",") (mapv read-string)))
(defn- fold [s]
  (let [[_ axis v] (re-find #"(x|y)=(\d+)" s)]
    (if (= axis "x") [(read-string v) -1] [-1 (read-string v)])))

(defn- fold-fn [[on-x on-y]]
  (fn [[x y]]
    (if (pos? on-y) ;; HINT: impossible to fold on 0 
      (let [max-y (-> on-y (* 2) inc)]
        (when (and (< y max-y) (not= y on-y))
          (if (< y on-y)
            [x y]
            [x (- (dec max-y) y)])))
      (let [max-x (-> on-x (* 2) inc)]
        (when (and (< x max-x) (not= x on-x))
          (if (< x on-x)
            [x y]
            [(- (dec max-x) x) y]))))))

(defn- part1 [folds coords]
  (->> (map (fold-fn (first folds)) coords)
       (into #{})
       (count)))

(let [sample (and nil test-sample)
      [scoords _ sfolds] (->> (or sample (slurp "input13.txt"))
                              (str/split-lines)
                              (partition-by empty?))
      coords (map coord scoords)
      folds (map fold sfolds)]
  (println "Part 1:" (part1 folds coords)))
