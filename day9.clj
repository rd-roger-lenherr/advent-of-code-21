(ns day9
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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
  (let [expected (inc (get-in matrix ref-cord))]
    (when (and (< expected 9) (= expected (get-in matrix cord)))
      cord)))

(defn- basin [matrix found cord]
  (let [check-next (some->> (neighbors+ matrix cord)
                            (map (partial risen? matrix cord))
                            (remove nil?))
        found-next (conj found cord)]
    (if (empty? check-next)
      found-next
      (reduce (fn [found cord] (basin matrix found cord))
              found-next check-next))))

(defn- part2 [matrix]
  (->> matrix
       (minima)
       (map (comp #_count set (partial basin matrix '())))
       ;; (sort >)
       ;; (take 3)
       #_(apply *)))

(let [matrix (->> (or test-sample (slurp "input9.txt"))
                  str/split-lines
                  lines->matrix)]
  ;; (println "Part 1:" (part1 matrix))
  ;; (println "Part 2:" (part2 matrix))
  (println :IS)
  (clojure.pprint/pprint (->> (part2 matrix)
                              (reduce into #{})
                              (map (partial get-in matrix))
                              (frequencies)
                              (into (sorted-map))))
  (clojure.pprint/pprint (dissoc (into (sorted-map) (frequencies (flatten matrix))) 9))
  (println :SHOULD)
  )


