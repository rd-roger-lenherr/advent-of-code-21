(ns day5)

(require '[clojure.string :as str])

(defn- inputs
  [s]
  (->> (re-seq #"(\d+)" s)
       (map (comp #(Integer/parseInt %) second))
       (partition 2)
       (partition 2)))

(defn- pair->instruction
  [[[p1x p1y] [p2x p2y]]]
  {:horz  (sorted-set p1x p2x)
   :vert  (sorted-set p1y p2y)
   :rise? (or (and (< p1x p2x) (> p1y p2y))
              (and (< p2x p1x) (> p2y p1y)))})

(defn- normalize
  [{:keys [horz vert rise?] :as _instruction}]
  (let [offset-x (apply min horz)
        offset-y (apply min vert)
        max-x    (apply max horz)
        max-y    (apply max vert)]
    {:origin [offset-x offset-y]
     :horz   (sorted-set 0 (- max-x offset-x))
     :vert   (sorted-set 0 (- max-y offset-y))
     :rise?  rise?}))

(defn- quadratic?
  [instruction]
  (let [{:keys [horz vert]} (normalize instruction)]
    (= horz vert)))

(defn- plot
  [chart {:keys [horz vert] :as instruction}]
  (let [discard-some? (quadratic? instruction)]
    (->> (for [line (range (apply min horz) (inc (apply max horz)))
               col  (range (apply min vert) (inc (apply max vert)))]
           [line col])
         (reduce #(update-in %1 %2 (fnil inc 0)) chart))))

(defn- block?
  [{:keys [horz vert] :as _instruction}]
  (= [2 2] [(count horz) (count vert)]))

(defn- instructions->score
  [instructions]
  (->> instructions
       (reduce plot {})
       (vals)
       (map vals)
       (flatten)
       (filter #(> % 1))
       (count)))

(defn- part1
  [pairs]
  (->> pairs
       (map pair->instruction)
       (remove block?)
       (instructions->score)))

(defn- diag->dots
  [instruction]
  (let [{:keys [origin horz rise?]} (normalize instruction)
        upper (apply max horz)
        lower (apply min horz)
        [offset-x offset-y] origin]
    (for [i (range (inc upper))
          :let [h (+ i offset-x)
                v (if rise?
                    (+ (- upper i) offset-y)
                    (+ i offset-y))]]
      {:horz (sorted-set h) :vert (sorted-set v)})))

(defn- diagonals-and-lines
  [instructions]
  (->> (filter quadratic? instructions)
       (map diag->dots)
       (flatten)
       (concat instructions)
       (remove block?)))

(defn- part2
  [pairs]
  (->> pairs
       (map pair->instruction)
       (diagonals-and-lines)
       (instructions->score)))

(let [given (->> (slurp "input5.txt"
)
                 (inputs))]
  (println (part1 given))
  (println (part2 given)))
