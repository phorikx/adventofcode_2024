(ns adventofcode.10b)
(require '[clojure.string :as str])
(require '[clojure.set :as s])

(defn print-grid [grid] (mapv #(println %) grid))
(defn vec-plus [coord-1 coord-2]
  [(+ (get coord-1 0) (get coord-2 0)) (+ (get coord-1 1) (get coord-2 1))])

(defn get-2d [grid coord] (get (get grid (get coord 0)) (get coord 1)))

(defn neighbors [location]
  [(vec-plus location [0 1]) (vec-plus location [0 -1]) (vec-plus location [1 0]) (vec-plus location [-1 0])])

(defn correct-neighbors [location value grid]
  (filter #(= (+ value 1) (get-2d grid %)) (neighbors location)))

(def count-paths (memoize (fn [location value grid]
                            (if (= 9 value) 1 (reduce + (mapv #(count-paths % (+ 1 value) grid) (correct-neighbors location value grid)))))))

(defn count-location [location grid] (if (= 0 (get-2d grid location)) (count-paths location 0 grid) 0))

(defn create-grid [input]
  (let [lines (str/split-lines input)
        chars (mapv #(seq (char-array %)) lines)
        grid (mapv (fn [line] (mapv #(Character/digit % 10) line)) chars)]
    grid))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_10.txt")
        input (slurp input-file)
        grid (create-grid input)
        inputs (for [i (range (count grid)) j (range (count (get grid 0)))] [i j])
        totals (mapv #(count-location % grid) inputs)
        total (reduce + totals)]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
