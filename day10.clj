(ns day10
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; Typing characters is inconvenient => some helpers
(def pairs (->> ["()" "{}" "[]" "<>"]
                (reduce (fn [m [open close]] (assoc m open close)) {})))
(def close->open (set/map-invert pairs))
(def open? (set (keys pairs)))
(def close? (set (vals pairs)))
(def incomplete? (partial every? open?))
(def broken? (partial some close?))

(defn- parse
  ([cs] (parse (list) cs))
  ([stack [c & cs]]
   (cond
     (nil? c) stack
     (open? c) (parse (conj stack c) cs)
     (and (close? c)) (if (= (close->open c) (first stack))
                        (parse (rest stack) cs)
                        (conj stack c)))))

(defn part1 [lines]
  (let [score {\) 3 \] 57 \} 1197 \> 25137}]
    (->> (map parse lines)
         (remove incomplete?)
         (map (comp score first))
         (reduce +))))

(defn part2 [lines]
  (let [score {\) 1 \] 2 \} 3 \> 4}]
    (->> (map parse lines)
         (remove broken?)
         (map (comp
               (partial reduce #(+ (* 5 %1) %2) 0)
               (partial map (comp score pairs))))
         (sort)
         (#(drop (int (/ (count %) 2)) %))
         (first))))

(let [lines (->> (slurp "input10.txt")
                 str/split-lines)]
  (println (part1 lines))
  (println (part2 lines)))
