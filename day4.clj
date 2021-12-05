(ns day4)
(require '[clojure.string :as str])

(def marker -1)

(defn- lines->board
  [lines]
  (->> (take 5 lines)
       (mapv (comp (partial mapv #(Integer/parseInt %)) #(str/split % #"\s+") str/trim))))

(defn- game-setup
  [lines]
  (let [numbers (first lines)
        boards  (->> (drop 2 lines)
                     (partition-all 6))]
    {:numbers (->> (str/split numbers #",")
                   (map #(Integer/parseInt %)))
     :boards  (map lines->board boards)}))

(defn- lookup
  [x board]
  (->> (for [row (range (count board))
             column (range (count (first board)))
             :let [path [row column]
                   found? (= x (get-in board path))]]
         (when found? path))
       (filter vector?)))

(defn- mark
  [x board]
  (let [[path] (lookup x board)]
    (cond-> board
      (seq path) (assoc-in path marker))))

(defn- transpose
  [board]
  (let [columns-max (count (first board))]
    (->> (for [col (range columns-max)
               row (range (count board))]
           (get-in board [row col]))
         (partition columns-max)
         (mapv vec))))

(defn- winner?
  [board]
  (->> (concat board (transpose board))
       (map (partial remove #{marker}))
       (some empty?)))

(defn- uncalled
  [board]
  (->> (flatten board)
       (remove #{marker})))

(defn- play
  [numbers boards]
  (let [drawn (first numbers)
        next-boards (map (partial mark drawn) boards)]
    (if-let [winning-board (first (filter winner? next-boards))]
      {:winning-number drawn :winning-board winning-board}
      (play (rest numbers) next-boards))))

(defn- part1
  [{:keys [numbers boards]}]
  (as-> (play numbers boards) %
    (update % :winning-board (comp (partial apply +) uncalled))
    (vals %)
    (apply * %)))

(defn- part2
  [{:keys [numbers boards]}]
  (let [drawn (first numbers)
        marked-boards (map (partial mark drawn) boards)
        next-boards (remove winner? marked-boards)]
    (if (empty? next-boards)
      (->> (first marked-boards)
           (uncalled)
           (apply +)
           (* drawn))
      (part2 {:numbers (rest numbers) :boards next-boards}))))

(let [game (->> (slurp "input4.txt")
                (str/split-lines)
                (game-setup))]
  (println (part1 game))
  (println (part2 game)))
