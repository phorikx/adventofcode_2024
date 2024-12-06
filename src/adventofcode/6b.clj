(ns adventofcode.6b)
(require '[clojure.string :as str])

(defn print-grid [grid] (mapv #(println %) grid) (println ""))

(def dirs [[-1 0] [0 1] [1 0] [0 -1]])

(defn get-2d [grid coord] (get (get grid (get coord 0)) (get coord 1)))
(defn vec-plus [coord-1 coord-2]
  [(+ (get coord-1 0) (get coord-2 0)) (+ (get coord-1 1) (get coord-2 1))])

(defn find-position [grid coord char]
  (let [size (count (get grid 0))
        next-coord (if (= size (get coord 1)) [(+ (get coord 0) 1) 0] (vec-plus coord [0 1]))]
    (if (= (get-2d grid coord) char) coord (recur grid next-coord char))))

(defn find-start-position [grid] (find-position grid [0 0] \^))

(defn trace-walk [grid coord direction previously-visited]
  (let [new-coord (vec-plus coord (get dirs direction))
        straight-coord (vec-plus new-coord (get dirs direction))
        contains_obstacle (= \# (get-2d grid straight-coord))
        new-dir (if contains_obstacle (mod (+ direction 1) 4) direction)
        ;; need to turn around completely if there is also a obstacle to the right
        contains_obstacle_after (= \# (get-2d grid (vec-plus new-coord (get dirs new-dir))))
        new-dir (if contains_obstacle_after (mod (+ new-dir 1) 4) new-dir)]
    (if (= nil (get-2d grid new-coord)) 0
        (if (contains? previously-visited [coord direction]) 1
            (let [updated-set (conj previously-visited [coord direction])]
              (recur grid new-coord new-dir updated-set))))))

(defn replace-location [grid coord]
  (assoc grid (get coord 0) (assoc (get grid (get coord 0)) (get coord 1) \#)))

(defn creates-loop [grid coord]
  (if (= \# (get-2d grid coord)) 0
      (if (= \^ (get-2d grid coord)) 0
          (let [start-position (find-start-position grid)
                new-grid (replace-location grid coord)
                start-direction (if (= \# (get-2d new-grid (vec-plus [-1 0] start-position))) 1 0)]
            (trace-walk new-grid start-position start-direction (set []))))))

(defn find-loops [grid]
  (mapv (fn [row] (map #(creates-loop grid [row %]) (range (count (get grid 0))))) (range (count grid))))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_6.txt")
        input (slurp input-file)
        grid (mapv #(into [] (seq (char-array %))) (str/split-lines input))
        valid-places (find-loops grid)
        total (reduce + (mapv #(reduce + %) valid-places))]
    (print-grid valid-places)
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
