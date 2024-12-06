(ns adventofcode.6b)
(require '[clojure.string :as str])

(def dirs [[0 -1] [1 0] [0 1] [-1 0]])

(defn get-2d [grid coord] (get (get grid (get coord 1)) (get coord 0)))
(defn vec-plus [coord-1 coord-2]
  [(+ (get coord-1 0) (get coord-2 0)) (+ (get coord-1 1) (get coord-2 1))])

(defn find-position [grid coord char]
  (let [size (count (get grid 0))
        next-coord (if (= size (get coord 1)) [(+ (get coord 0) 1) 0] (vec-plus coord [0 1]))]
    (if (= (get-2d grid coord) char) coord (recur grid next-coord char))))

(defn find-start-position [grid] (find-position grid [0 0] \^))

(defn trace-walk [grid coord direction]
  (println coord)
  (let [replaced-grid (assoc grid (get coord 1) (assoc (get grid (get coord 1)) (get coord 0) \x))
        new-coord (vec-plus coord (get dirs direction))
        straight-coord (vec-plus new-coord (get dirs direction))
        contains_obstacle (= \# (get-2d grid straight-coord))
        new-dir (if contains_obstacle (mod (+ direction 1) 4) direction)
        next-coord (vec-plus new-coord (get dirs new-dir))]
    (if (= nil (get-2d grid new-coord)) replaced-grid
        (recur replaced-grid new-coord new-dir))))

(defn count-x [grid] (reduce + (mapv (fn [row] (count (filter #(= \x %) row))) grid)))

(defn print-grid [grid] (mapv #(println %) grid))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_6.txt")
        input (slurp input-file)
        grid (mapv #(into [] (seq (char-array %))) (str/split-lines input))
        start-position (find-start-position grid)
        finished-grid (trace-walk grid start-position 0)
        count-positions (count-x finished-grid)]
    (print-grid finished-grid)
    (println count-positions)))

(when (= *command-line-args* ["run"])
  (-main))
