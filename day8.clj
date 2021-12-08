(ns day8
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn part1 [lines]
  (as-> lines it
    (map #(-> % (str/split #" \| ") second str/trim (str/split #" ")) it)
    (flatten it)
    (group-by count it)
    (select-keys it [2 3 4 7])
    (vals it)
    (flatten it)
    (count it)))

(defn line->input [s]
  (->> (str/split s #" \| ")
       (map #(->> (str/split % #" ") (map str/trim)))
       (zipmap [:signals :display])))

(defn find-delta [refsym delta xs]
  "Given a string `refsym` seeks for an element in `xs` with a difference in
  segements of `delta`. Performs a stringified version of binary AND followed by
  XOR."
  (->> xs
       (filter #((comp #{delta} count)
                 (apply set/difference (sort-by count > [(set refsym) (set %)]))))
       (first)))

(defn find-1 [signals] (first (filter (comp #{2} count) signals)))
(defn find-4 [signals] (first (filter (comp #{4} count) signals)))
(defn find-7 [signals] (first (filter (comp #{3} count) signals)))
(defn find-8 [signals] (first (filter (comp #{7} count) signals)))
(defn find-3 [signals] (let [no1     (find-1 signals)
                             group-5 (-> (group-by count signals)
                                         (get 5))]
                         (find-delta no1 3 group-5)))
(defn find-5 [signals] (let [no4     (find-4 signals)
                             group-5 (-> (group-by count signals)
                                         (get 5))]
                         (find-delta no4 2 (remove #{(find-3 signals)} group-5))))
(defn find-2 [signals] (let [group-5 (-> (group-by count signals)
                                         (get 5))]
                         (first (remove #{(find-3 signals) (find-5 signals)} group-5))))
(defn find-9 [signals] (let [group-6 (-> (group-by count signals)
                                         (get 6))]
                         (find-delta (find-4 signals) 2 group-6)))
(defn find-0 [signals] (let [group-6 (-> (group-by count signals)
                                         (get 6))]
                         (find-delta (find-1 signals) 4 (remove #{(find-9 signals)} group-6))))
(defn find-6 [signals] (let [group-6 (-> (group-by count signals)
                                         (get 6))]
                         (first (remove #{(find-9 signals) (find-0 signals)} group-6))))

(defn input->number [input]
  (let [chiffre (->> input
                     :signals
                     ((juxt find-0 find-1 find-2 find-3 find-4 find-5 find-6 find-7 find-8 find-9))
                     (map set)
                     (zipmap (range 0 10))
                     (set/map-invert))]
    (->> (:display input)
         (map (comp chiffre set))
         (str/join)
         (Integer/parseInt))))

(defn part2
  [lines]
  (->> (map (comp input->number line->input) lines)
       (reduce +)))

(let [lines (str/split-lines (slurp "input8.txt"))]
  (println (part1 lines))
  (println (part2 lines)))
