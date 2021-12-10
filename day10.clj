(ns day10
  (:require [clojure.string :as str]
            [clojure.zip :as z]
            [clojure.set :as set]))

(def test-sample
  "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

;; Typing characters is inconvenient => some helpers
(def pairs (->> ["()" "{}" "[]" "<>"]
                (reduce (fn [m [open close]] (assoc m open close)) {})))
(def close->open (set/map-invert pairs))
(def open? (set (keys pairs)))
(def close? (set (vals pairs)))
(def incomplete? (partial every? open?))
(def broken? (partial some? close?))
(def score {\) 3 \] 57 \} 1197 \> 25137})

(defn- parse
  ([cs] (parse (list) cs))
  ([stack [c & cs]]
   (cond
     (nil? c) stack
     (open? c) (parse (conj stack c) cs)
     (and (close? c)) (if (= (close->open c) (first stack))
                        (parse (rest stack) cs)
                        (conj stack c)))))

(let [lines (->> #_test-sample
                 (slurp "input10.txt")
                 str/split-lines)]
  (->> (map parse lines)
       (remove incomplete?)
       (map (comp score first))
       (reduce +)
       (clojure.pprint/pprint)))
