(ns day12
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn small? [k] (let [s (name k)] (= s (str/lower-case s))))
(def big? (complement small?))

(defn options
  [input path current]
  (let [visited (set (filter small? path))]
    (set/difference (get input current) visited)))

(defn options-2
  [input path current]
  (let [visited (filter small? (conj path current))
        double? (->> (frequencies visited)
                     (some (fn [[k v]] (when (#{2} v) k))))]
    (cond-> (get input current)
      double? (set/difference (set visited)))))

(defn chart
  ([choice-fn input] (chart choice-fn input '() '() :start))
  ([choice-fn input paths path current]
   (let [choices      (choice-fn input path current)
         current-path (conj path current)]
     (if (empty? choices)
       (conj paths current-path)
       (reduce (fn [paths choice]
                 (chart choice-fn input paths current-path choice))
               paths choices)))))

(defn solve
  [choice-fn input]
  (->> (chart choice-fn input)
       (filter (comp #{:end} first))
       ;; (map reverse)
       (count)))

(defn tuples->input
  [tuples]
  (reduce (fn [m [sk sv]]
            (let [k (keyword sk)
                  v (keyword sv)]
              (cond-> m
                (not= k :end) (update k (fnil conj #{}) v)
                (not= v :end) (update v (fnil conj #{}) k)
                (= v :start)  (update k set/difference #{:start})
                (= k :start)  (update v set/difference #{:start}))))
          {} tuples))

(def sample-s
  "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def sample-l
  "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")

(let [sample nil
      input (->> (or sample (slurp "input12.txt"))
                 (str/split-lines)
                 (map #(str/split % #"-"))
                 (tuples->input))]
  (println "Part 1:" (solve options input))
  (println "Part 2:" (solve options-2 input)))
