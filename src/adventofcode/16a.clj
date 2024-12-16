(ns adventofcode.16a)
(require '[clojure.string :as str])

(def dirs {\^ [-1 0]
           \v [1 0]
           \< [0 -1]
           \> [0 1]})

(defn vec-plus [coord-1 coord-2]
  [(+ (get coord-1 0) (get coord-2 0)) (+ (get coord-1 1) (get coord-2 1))])

(defn vec-times [k coord]
  [(* k (get coord 0)) (* k (get coord 1))])
(defn get-2d [grid coord] (get (get grid (get coord 0)) (get coord 1)))

(defn print-grid [grid] (mapv #(println %) grid))

(defn assoc-2d [grid coord val]
  (assoc grid (get coord 0) (assoc (get grid (get coord 0)) (get coord 1) val)))

(defn make-maze [input]
  (mapv #(into [] (seq (char-array %))) (str/split-lines (get (str/split input #"\n\n") 0))))

(defn find-path [maze] 0)

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_16.txt")
        input (slurp input-file)
        maze (make-maze input)
        total (find-path maze)]
    (print-grid maze)))

(when (= *command-line-args* ["run"])
  (-main))
