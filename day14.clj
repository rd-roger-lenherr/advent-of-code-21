(ns day14
  (:require [clojure.string :as str]))

(defn- ->rule [s] (str/split s #" -> "))
(defn- ->book [rules]
  (reduce (fn [m [from to]]
            (assoc m (seq from) (.charAt to 0)))
          {} rules))

(defn- pair->pairs [[a b :as pair] c]
  (if-not c
    (list pair)
    (list (list a c) (list c b))))

(defn- step [book pair-freqs]
  (reduce-kv (fn [pair-freqs pair cnt]
               (->> (pair->pairs pair (book pair))
                    (reduce #(assoc %1 %2 cnt) {})
                    (merge-with + pair-freqs)))
             {} pair-freqs))

(defn- pair-frequencies [pairs]
  (-> (frequencies pairs)
      (assoc (-> pairs first reverse vec (assoc 0 nil)) 1)))

(defn- pair-freq->letter-freq [freqs]
  (->> (map (juxt #(-> % first second) second) freqs)
       (reduce #(update %1 (first %2) (fnil + 0) (second %2)) {})))

(defn- solve [rulebook pairs iterations]
  (->> (range iterations)
       (reduce (fn [pair-freqs _]
                 (step rulebook pair-freqs))
               (pair-frequencies pairs))
       (pair-freq->letter-freq)
       (vals)
       (sort-by identity >)
       ((juxt first last))
       (apply -)))

(let [[[template] _ rules] (->> (slurp "input14.txt")
                                (str/split-lines)
                                (partition-by empty?))
      rulebook             (->> rules (map ->rule) ->book)
      pairs                (partition 2 1 template)]
  (println "Part 1:" (solve rulebook pairs 10))
  (println "Part 2:" (solve rulebook pairs 40)))
