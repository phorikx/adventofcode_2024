(ns adventofcode.18a)
(require '[clojure.string :as str])

(def grid-size 71)
(def no-of-instructions 1024)

(defn vec-plus [coord-1 coord-2]
  [(+ (get coord-1 0) (get coord-2 0)) (+ (get coord-1 1) (get coord-2 1))])

(defn neighbors [location]
  [(vec-plus location [0 1]) (vec-plus location [0 -1]) (vec-plus location [1 0]) (vec-plus location [-1 0])])

(defn vec-times [k coord]
  [(* k (get coord 0)) (* k (get coord 1))])
(defn get-2d [grid coord] (get (get grid (get coord 0)) (get coord 1)))

(defn print-grid [grid] (mapv #(println %) grid))

(defn assoc-2d [grid coord val]
  (assoc grid (get coord 0) (assoc (get grid (get coord 0)) (get coord 1) val)))

(defn make-coords [input]
  (mapv #(mapv (fn [coord] (Integer/parseInt coord)) (str/split % #",")) (str/split-lines input)))

(defn make-maze [grid falling-coords counter]
  (if (or (= 0 (count falling-coords)) (= counter no-of-instructions)) grid
      (recur (assoc-2d grid (first falling-coords) \#) (subvec falling-coords 1 (count falling-coords)) (+ 1 counter))))

(defn get-lowest-helper [some-map lowest k]
  (if (= 0 (count some-map)) [k lowest]
      (let [current (last some-map)
            is-lower (or (< (get current 1) lowest) (= -1 lowest))
            lowest (if is-lower (get current 1) lowest)
            k (if is-lower (get current 0) k)]
        (recur (butlast some-map) lowest k))))

(defn get-lowest [some-map] (get-lowest-helper some-map -1 []))

(defn add-point-helper [current-point already-visited maze neighbors]
  (if (= 0 (count neighbors)) already-visited
      (let [current (last neighbors)
            in-maze (get-2d maze current)
            invalid (or (= nil in-maze) (= \# in-maze))
            new-val (+ 1 (get current-point 1))
            lower-value (if (= nil (already-visited current)) new-val (min new-val (already-visited current)))
            already-visited (if invalid
                              already-visited
                              (assoc already-visited current lower-value))]
        (recur current-point already-visited maze (butlast neighbors)))))

(defn add-points [current-point already-visited maze]
  (add-point-helper current-point already-visited maze (neighbors (get current-point 0))))

(defn remove-doubles [already-visited known-shortest]
  (apply dissoc already-visited (into [] (keys known-shortest))))

(defn find-path [known-shortest already-visited end-point maze]
  (let [current-point (get-lowest already-visited)
        point (get current-point 0)
        length (get current-point 1)]
    (if (= end-point point) length
        (let [known-shortest (assoc known-shortest point length)
              already-visited (add-points current-point already-visited maze)
              already-visited (remove-doubles already-visited known-shortest)]
          (recur known-shortest already-visited end-point maze)))))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_18.txt")
        input (slurp input-file)
        falling-coords (make-coords input)
        grid (into [] (for [_ (range grid-size)] (into [] (repeat grid-size \.))))
        maze (make-maze grid falling-coords 0)
        shortest (find-path {} {[0 0] 0} [(- grid-size 1) (- grid-size 1)] maze)]
    (print-grid maze)
    (println shortest)))

(when (= *command-line-args* ["run"])
  (-main))
