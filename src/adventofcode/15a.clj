(ns adventofcode.15a
  (:require
   [clojure.string :as str]))
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

(defn assoc-2d [grid coord val]
  (assoc grid (get coord 0) (assoc (get grid (get coord 0)) (get coord 1) val)))

(defn make-warehouse [input]
  (mapv #(into [] (seq (char-array %))) (str/split-lines (get (str/split input #"\n\n") 0))))

(defn make-robot-moves [input]
  (into [] (reduce concat (mapv #(into [] (seq (char-array %))) (str/split-lines (get (str/split input #"\n\n") 1))))))

(defn find-robot-helper [warehouse coord]
  (if (= \@ (get-2d warehouse coord)) coord
      (let [first-coord (if (= (- (count (get warehouse 0)) 1) (get coord 1)) (+ 1 (get coord 0)) (get coord 0))
            second-coord (if (= (- (count (get warehouse 0)) 1) (get coord 1)) 0 (+ 1 (get coord 1)))]
        (recur warehouse [first-coord second-coord]))))

(defn find-robot [warehouse] (find-robot-helper warehouse [0 0]))

(defn score-helper [warehouse total coords-to-check]
  (if (= 0 (count coords-to-check)) total
      (let [current-coord (last coords-to-check)
            next-coords (butlast coords-to-check)
            score-current (if (= \O (get-2d warehouse current-coord)) (+ (* 100 (get current-coord 0)) (get current-coord 1)) 0)]
        (recur warehouse (+ total score-current) next-coords))))

(defn score-warehouse [warehouse] (score-helper warehouse 0
                                                (for [i (range (count warehouse)) j (range (count (get warehouse 0)))] [i j])))

(defn swap-warehouse [warehouse coord-1 coord-2]
  (let [pos1 (get-2d warehouse coord-1)
        pos2 (get-2d warehouse coord-2)]
    (assoc-2d (assoc-2d warehouse coord-2 pos1) coord-1 pos2)))

(def try-move (fn [current-move warehouse position]
                (let [coords-one-further (vec-plus position (dirs current-move))
                      on-one-further (get-2d warehouse coords-one-further)]
                  (if (= \# on-one-further) [false warehouse]
                      (if (= \. on-one-further) [true (swap-warehouse warehouse position coords-one-further)]
                          (let [[could-move updated-warehouse] (try-move current-move warehouse coords-one-further)]
                            (if (not could-move) [false warehouse]
                                [true (swap-warehouse updated-warehouse position coords-one-further)])))))))

(defn update-warehouse [current-move warehouse robot-position]
  (let [[could-move updated-warehouse] (try-move current-move warehouse robot-position)
        next-robot-position (if could-move (vec-plus (dirs current-move) robot-position) robot-position)]
    [updated-warehouse next-robot-position]))

(defn move-robot [robot-moves warehouse robot-position]
  (if (= 0 (count robot-moves)) warehouse
      (let [current-move (first robot-moves)
            next-moves (subvec robot-moves 1 (count robot-moves))
            [next-warehouse next-robot-position] (update-warehouse current-move warehouse robot-position)]
        (recur next-moves next-warehouse next-robot-position))))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_15.txt")
        input (slurp input-file)
        warehouse (make-warehouse input)
        robot-moves (make-robot-moves input)
        robot-position (find-robot warehouse)
        warehouse-end (move-robot robot-moves warehouse robot-position)
        total (score-warehouse warehouse-end)]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
