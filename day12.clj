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
  ([choice-fn input] (chart choice-fn input (list) (list) :start))
  ([choice-fn input paths path current]
   (let [choices (choice-fn input path current)
         current-path (conj path current)]
     (if (empty? choices)
       (conj paths current-path)
       (reduce (fn [ps choice]
                 (chart choice-fn input ps current-path choice))
               paths choices)))))

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

(defn- solve
  [choice-fn input]
  (->> (chart choice-fn input)
       (filter (comp #{:end} first))
       (#(doto % (->> (map reverse) (group-by second) clojure.pprint/pprint)))
       (count)))

(defn options-2
  [input path current]
  (let [double? (->> (frequencies path)
                     (some (fn [[k v]] (when (#{2} v) k))))]
    (cond-> (get input current)
      double? (set/difference (->> path (filter small?) set)))))

(let [sample test-sample
      input (->> (or sample (slurp "input12.txt"))
                 (str/split-lines)
                 (map #(str/split % #"-"))
                 (tuples->input))]
  ;; (println "Part 1:" (solve options input))
  (println "Part 2:" (solve options-2 input)))


(set/difference
 (->>
  "start,A,b,A,b,A,c,A,end
start,A,b,A,b,A,end
start,A,b,A,b,end
start,A,b,A,c,A,b,A,end
start,A,b,A,c,A,b,end
start,A,b,A,c,A,c,A,end
start,A,b,A,c,A,end
start,A,b,A,end
start,A,b,d,b,A,c,A,end
start,A,b,d,b,A,end
start,A,b,d,b,end
start,A,b,end
start,A,c,A,b,A,b,A,end
start,A,c,A,b,A,b,end
start,A,c,A,b,A,c,A,end
start,A,c,A,b,A,end
start,A,c,A,b,d,b,A,end
start,A,c,A,b,d,b,end
start,A,c,A,b,end
start,A,c,A,c,A,b,A,end
start,A,c,A,c,A,b,end
start,A,c,A,c,A,end
start,A,c,A,end
start,A,end"
  (str/split-lines)
  (map #(str/split % #","))
  (map (partial map keyword))
  (into #{}))

 (into #{}
          '((:start :A :end)
            (:start :A :b :end)
            (:start :A :b :d :b :end)
            (:start :A :b :d :b :A :end)
            (:start :A :b :d :b :A :c :A :end)
            (:start :A :b :A :end)
            (:start :A :b :A :b :end)
            (:start :A :b :A :b :A :end)
            (:start :A :b :A :b :A :c :A :end)
            (:start :A :b :A :c :A :end)
            (:start :A :c :A :end)
            (:start :A :c :A :b :end)
            (:start :A :c :A :b :A :end)
            (:start :A :c :A :c :A :end)
            (:start :A :c :A :c :A :b :end)
            (:start :A :c :A :c :A :b :A :end))))
