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

(defn- step
  [book m]
  (reduce-kv (fn [m pair cnt]
               (->>  (insert book [pair])
                     (reduce #(assoc %1 %2 cnt) {})
                    (merge-with + m)))
             {} m))

(defn- init [pairs]
  (-> (frequencies pairs)
      (assoc (-> pairs first reverse vec (assoc 0 nil)) 1)))

(defn- reducepairs [freqs]
  (->> freqs
       (map (juxt #(-> % first second) second))
       (reduce #(update %1 (first %2) (fnil + 0) (second %2)) {})))

(defn- part2 [rulebook pairs]
  (->> (range 40)
       (reduce (fn [m _]
                 (step rulebook m))
               (init pairs))
       (reducepairs)
       ;; same as part 1 now
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
  (->> #_(reduce (fn [m p] (update m p (fnil inc 0))) {} pairs)
       ;; (step rulebook (init pairs))
       (part2 rulebook pairs)
       ;; (map (juxt #(-> % first second) second))
       ;; (reduce #(update %1 (first %2) (fnil + 0) (second %2)) {})
       (println)
       #_eof))
