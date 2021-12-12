(ns day9
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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
    [y x]))

(defn- part1 [matrix]
  (->> matrix
       minima
       (map (comp inc (partial get-in matrix)))
       (reduce +)))

(defn- neighbors+ [matrix [yref xref]]
  (for [y (range (max 0 (dec yref)) (inc (min (inc yref) (dec (count matrix)))))
        x (range (max 0 (dec xref)) (inc (min (inc xref) (dec (count (first matrix))))))
        :when (or (and (= y yref) (not= x xref))
                  (and (= x xref) (not= y yref)))]
    [y x]))

(defn- risen? [matrix ref-cord cord]
  (let [prev   (get-in matrix ref-cord)
        gotten (get-in matrix cord)]
    (when (and (< gotten 9) (< prev gotten))
      cord)))

(defn- basin [matrix found cord]
  (let [check-next (some->> (neighbors+ matrix cord)
                            (filter (partial risen? matrix cord)))
        found-next (conj found cord)]
    (if (empty? check-next)
      found-next
      (reduce (fn [found cord] (basin matrix found cord))
              found-next check-next))))

(defn- part2 [matrix]
  (->> matrix
       (minima)
       (map (comp count set (partial basin matrix '())))
       (sort >)
       (take 3)
       (apply *)))

(let [matrix (->> (slurp "input9.txt")
                  str/split-lines
                  lines->matrix)]
  (println "Part 1:" (part1 matrix))
  (println "Part 2:" (part2 matrix)))


