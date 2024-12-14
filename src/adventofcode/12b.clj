(ns adventofcode.12b)
(require '[clojure.string :as str])
(require '[clojure.set :as s])

(defn vec-plus [coord-1 coord-2]
  [(+ (get coord-1 0) (get coord-2 0)) (+ (get coord-1 1) (get coord-2 1))])

(defn get-2d [grid coord] (get (get grid (get coord 0)) (get coord 1)))
(defn get-2d-value [grid coord] (get (get-2d grid coord) 0))
(defn get-already-counted [grid coord] (get (get-2d grid coord) 1))

(defn neighbors [location]
  [(vec-plus location [0 1]) (vec-plus location [0 -1]) (vec-plus location [1 0]) (vec-plus location [-1 0])])

(def correct-neighbors (memoize (fn [location grid]
                                  (filter #(= (get-2d-value grid location) (get-2d-value grid %)) (neighbors location)))))

(defn incorrect-neighbors [location grid]
  (let [all-neighbors (neighbors location)
        neighbors-with-direction (mapv vector '(\R \L \D \U) all-neighbors)
        outside-neighbors (filter #(not (contains? (into #{} (correct-neighbors location grid)) (get % 1))) neighbors-with-direction)]
    outside-neighbors))

(defn perimeter [location grid] (- 4 (count (correct-neighbors location grid))))

(defn get-area-helper [to-check already-checked grid outside-neighbors]
  (if (= 0 (count to-check)) [already-checked outside-neighbors]
      (let [currently-checking (first to-check)
            checked-already (contains? already-checked currently-checking)
            to-check (disj to-check currently-checking)
            to-check (if checked-already to-check (s/union (into #{} (correct-neighbors currently-checking grid)) to-check))
            already-checked (conj already-checked currently-checking)
            outside-neighbors (s/union outside-neighbors (into #{} (incorrect-neighbors currently-checking grid)))]
        (recur to-check already-checked grid outside-neighbors))))

(defn get-area [start-point grid]
  (get-area-helper (into #{} (correct-neighbors start-point grid)) #{start-point} grid #{}))

(defn set-to-visited [grid location]
  (assoc grid (get location 0)
         (assoc (get grid (get location 0)) (get location 1) [(get-2d-value grid location) true])))

(defn update-grid [area grid]
  (reduce #(set-to-visited %1 %2) grid area))

(defn check-neighbor [to-check outside-neighbors]
  (let [direction (get to-check 0)
        to-bottom (vec-plus (get to-check 1) [1 0])
        to-right (vec-plus (get to-check 1) [0 1])]
    (if (or (= direction \U) (= direction \D)) (contains? outside-neighbors [direction to-right])
        (contains? outside-neighbors [direction to-bottom]))))

(defn reduce-outside-helper [to-check outside-neighbors total]
  (if (= 0 (count to-check)) total
      (let [currently-checking (last to-check)
            contains-neighbor (check-neighbor currently-checking outside-neighbors)]
        (recur (butlast to-check) outside-neighbors (if contains-neighbor (+ 1 total) total)))))

(defn reduce-outside [outside-neighbors] (reduce-outside-helper outside-neighbors outside-neighbors 0))

(defn count-region-helper [coords grid total]
  (if (or (= 0 (count coords)) (= nil coords)) total
      (let [counted-already (get-already-counted grid (last coords))
            start-point (last coords)
            [area outside-neighbors] (if counted-already nil (get-area start-point grid))
            perim (if counted-already nil (reduce + (mapv #(perimeter % grid) area)))
            no_of_doubles (if counted-already nil (reduce-outside outside-neighbors))
            sides (if counted-already nil (- perim no_of_doubles))
            new-total (if counted-already total (+ total (* sides (count area))))
            new-grid (if counted-already grid (update-grid area grid))]
        (recur (butlast coords) new-grid new-total))))

(defn count-regions [coords grid]
  (count-region-helper coords grid 0))

(defn create-grid [input]
  (let [lines (str/split-lines input)
        characters (mapv #(seq (char-array %)) lines)
        grid (mapv (fn [row] (mapv #(vector % false) row)) characters)]

    grid))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_12.txt")
        input (slurp input-file)
        grid (create-grid input)
        inputs (for [i (range (count grid)) j (range (count (get grid 0)))] [i j])
        outside-neighbors (incorrect-neighbors (get (into [] inputs) 0) grid)
        totals (count-regions inputs grid)]
    (println totals)))

(when (= *command-line-args* ["run"])
  (-main))
