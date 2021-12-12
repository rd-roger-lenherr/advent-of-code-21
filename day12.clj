(ns day12
  (:require [clojure.string :as str]))

(def test-sample
  "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(defn- small? [k] (let [s (name k)] (= s (str/lower-case s))))
(def big? (complement small?))

(defn- chart [input]
  )

(let [debug? true
      input (->> (if debug? test-sample (slurp "input12.txt"))
                 (str/split-lines)
                 (map #(str/split % #"-"))
                 (reduce (fn [m [k v]] (update m (keyword k) (fnil conj #{}) (keyword v))) {}))]
  (->> input
       #_eof))
