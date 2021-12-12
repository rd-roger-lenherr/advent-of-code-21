(ns day9
  (:require [clojure.string :as str]))

(def test-sample
  "2199943210
3987894921
9856789892
8767896789
9899965678")

(def lines->matrix
  (partial mapv (comp (partial mapv (comp read-string str)) seq)))

(defn- minima?
  [[y x :as cself] matrix]
  (let [self (get-in matrix cself)
        in #(get-in matrix % 9)]
    (< self (min (in [(dec y) x])
                 (in [(inc y) x])
                 (in [y (dec x)])
                 (in [y (inc x)])))))

(defn- minima [matrix]
  (for [y (range (count matrix))
        x (range (count (first matrix)))
        :when (minima? [y x] matrix)]
    (get-in matrix [y x])))

(defn- part1 [matrix]
  (->> matrix
       minima
       (map inc)
       (reduce +)))

(let [matrix (->> (slurp "input9.txt")
                  str/split-lines
                  lines->matrix)]
  (println "Part 1:" (part1 matrix)))


