(ns day14
  (:require [clojure.string :as str]))

(def test-sample
  "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(defn- ->rule [s] (str/split s #" -> "))

(defn- rules->book [pairs]
  (reduce (fn [m [from to]] (assoc m (seq from) (.charAt to 0))) {} pairs))

(defn- pair->pairs [[a b :as pair] c]
  (if-not c
    (list pair)
    (list (list a c) (list c b))))

(defn- insert [book pairs]
  (println book pairs)
  (mapcat #(pair->pairs % (book %)) pairs))

(defn unpair [[lead & others]]
  (->> (map second others)
       (concat lead)))

(defn- part1 [book pairs]
  (->> (reduce (fn [ps _] (insert book ps)) pairs (range 10))
       (unpair)
       (frequencies)
       (vals)
       (sort-by identity >)
       ((juxt first last))
       (apply -)))

(let [sample               (comment test-sample)
      [[template] _ rules] (->> (or sample (slurp "input14.txt"))
                                (str/split-lines)
                                (partition-by empty?))
      rulebook             (->> rules
                              (map ->rule)
                              rules->book)
      pairs                (partition 2 1 template)]
  (->> (part1 rulebook pairs)
       #_eof))
