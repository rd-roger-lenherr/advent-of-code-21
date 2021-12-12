(ns day12
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def test-sample
  "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def test-sample-2
  "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")

(defn- small? [k] (let [s (name k)] (= s (str/lower-case s))))
(def big? (complement small?))

(defn options
  [input path current]
  (let [taken (set (filter small? path))]
    (set/difference (get input current) taken)))

(defn- chart
  ([input] (chart input (list) (list) :start))
  ([input paths path current]
   (let [choices (options input path current)
         current-path (conj path current)]
     (if (empty? choices)
       (conj paths current-path)
       (reduce (fn [ps choice]
                 (chart input ps current-path choice))
               paths choices)))))

(defn tuples->input
  [tuples]
  (-> (reduce (fn [m [k v]]
                (-> m
                    (update (keyword k) (fnil conj #{}) (keyword v))
                    (update (keyword v) (fnil conj #{}) (keyword k))))
              {} tuples)
      (dissoc :end)))

(defn- part1
  [input]
  (->> (chart input)
       (filter (comp #{:end} first))
       (count)))

(let [sample test-sample
      input (->> (or sample (slurp "input12.txt"))
                 (str/split-lines)
                 (map #(str/split % #"-"))
                 (tuples->input))]
  (println (part1 input))
  #_(-> input
      ;; (doto (println))
      (chart)
      #_eof))
