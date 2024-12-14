(ns adventofcode.14b)
(require '[clojure.string :as str])

(def max-x 101)
(def max-y 103)
(defn print-grid [grid] (mapv #(println %) grid))

(defn vec-plus [coord-1 coord-2]
  [(+ (get coord-1 0) (get coord-2 0)) (+ (get coord-1 1) (get coord-2 1))])

(defn vec-times [k coord]
  [(* k (get coord 0)) (* k (get coord 1))])
(defn get-2d [grid coord] (get (get grid (get coord 0)) (get coord 1)))

(defn wrap-position [coord] [(mod (get coord 0) max-x) (mod (get coord 1) max-y)])

(defn make-robots [input]
  (let [robots (str/split-lines input)
        robots (mapv (fn [line] (re-matches #"[^\d]+(\d+),(\d+)[^\d\-]+([\-\d]+),([\-\d]+)" line)) robots)
        robots (mapv #(subvec % 1 5) robots)
        robots (mapv #(mapv Integer/parseInt %) robots)
        robots (mapv (fn [robot] {:start (subvec robot 0 2) :velocity (subvec robot 2 4)}) robots)]
    robots))

(defn move-robot [robot n]
  (wrap-position (vec-plus (robot :start) (vec-times n (robot :velocity)))))

(defn make-grid [robots]
  (for [i (range max-y)] (for [j (range max-x)]
                           (if (contains? (into #{} robots) [j i]) "*" " "))))

(defn picture-loop [robots n]
  (let [pic-robots (mapv #(move-robot % n) robots)
        _ (println n)
        grid (make-grid pic-robots)
        _ (print-grid grid)
        _ (read-line)]
    (recur robots (+ 1 n))))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_14.txt")
        input (slurp input-file)
        robots (make-robots input)
        picture (picture-loop robots 6240)]
    (println picture)))

(when (= *command-line-args* ["run"])
  (-main))
