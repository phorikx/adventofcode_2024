(ns adventofcode.15b)
(require '[clojure.string :as str])
(declare try-move-box)

(def dirs {\^ [-1 0]
           \v [1 0]
           \< [0 -1]
           \> [0 1]})

(def doubling {\. '(\. \.)
               \# '(\# \#)
               \O '(\[ \])
               \@ '(\@ \.)})

(defn vec-plus [coord-1 coord-2]
  [(+ (get coord-1 0) (get coord-2 0)) (+ (get coord-1 1) (get coord-2 1))])
(defn print-grid [grid] (mapv #(println %) grid))

(defn vec-times [k coord]
  [(* k (get coord 0)) (* k (get coord 1))])
(defn get-2d [grid coord] (get (get grid (get coord 0)) (get coord 1)))

(defn assoc-2d [grid coord val]
  (assoc grid (get coord 0) (assoc (get grid (get coord 0)) (get coord 1) val)))

(defn make-warehouse [input]
  (let [spaces (mapv #(into [] (seq (char-array %))) (str/split-lines (get (str/split input #"\n\n") 0)))
        doubled (mapv (fn [row] (mapv (fn [spot] (doubling spot)) row)) spaces)
        normal (mapv (fn [row] (into [] (reduce concat row))) doubled)]
    normal))

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
            score-current (if (= \[ (get-2d warehouse current-coord)) (+ (* 100 (get current-coord 0)) (get current-coord 1)) 0)]
        (recur warehouse (+ total score-current) next-coords))))

(defn score-warehouse [warehouse] (score-helper warehouse 0
                                                (for [i (range (count warehouse)) j (range (count (get warehouse 0)))] [i j])))

(defn swap-warehouse [warehouse coord-1 coord-2]
  (let [pos1 (get-2d warehouse coord-1)
        pos2 (get-2d warehouse coord-2)]
    (assoc-2d (assoc-2d warehouse coord-2 pos1) coord-1 pos2)))

(defn move-box [warehouse left-coord dir]
  (case dir
    \< (-> warehouse (assoc-2d left-coord \]) (assoc-2d (vec-plus left-coord [0 1]) \.) (assoc-2d (vec-plus left-coord [0 -1]) \[))
    \> (-> warehouse (assoc-2d left-coord \.) (assoc-2d (vec-plus left-coord [0 1]) \[) (assoc-2d (vec-plus left-coord [0 2]) \]))
    \^ (-> warehouse (assoc-2d left-coord \.)
           (assoc-2d (vec-plus left-coord [0 1]) \.)
           (assoc-2d (vec-plus left-coord [-1 0]) \[)
           (assoc-2d (vec-plus left-coord [-1 1]) \]))
    \v (-> warehouse (assoc-2d left-coord \.)
           (assoc-2d (vec-plus left-coord [0 1]) \.)
           (assoc-2d (vec-plus left-coord [1 0]) \[)
           (assoc-2d (vec-plus left-coord [1 1]) \]))))

(defn check-above [warehouse coord is-left]
  (case (get-2d warehouse (vec-plus coord [-1 0]))
    \# [false warehouse]
    \. [true warehouse]
    \[ (try-move-box warehouse (vec-plus coord [-1 0]) \^)
    \] (if is-left (try-move-box warehouse (vec-plus coord [-1 -1]) \^) [true warehouse])))

(defn check-below [warehouse coord is-left]
  (case (get-2d warehouse (vec-plus coord [1 0]))
    \# [false warehouse]
    \. [true warehouse]
    \[ (try-move-box warehouse (vec-plus coord [1 0]) \v)
    \] (if is-left (try-move-box warehouse (vec-plus coord [1 -1]) \v) [true warehouse])))

(def try-move-box
  (fn [warehouse left-coord dir]
    (case dir
      \< (case (get-2d warehouse (vec-plus left-coord (dirs dir)))
           \. [true (move-box warehouse left-coord dir)]
           \# [false warehouse]
           \] (let [[could-move new-warehouse] (try-move-box warehouse (vec-plus left-coord [0 -2]) dir)]
                (if could-move [true (move-box new-warehouse left-coord dir)] [false warehouse])))
      \> (case (get-2d warehouse (vec-plus left-coord [0 2]))
           \. [true (move-box warehouse left-coord dir)]
           \# [false warehouse]
           \[ (let
               [[could-move new-warehouse] (try-move-box warehouse (vec-plus left-coord [0 2]) dir)]
                (if could-move [true (move-box new-warehouse left-coord dir)] [false warehouse])))
      \^ (let [[could-move-left new-warehouse] (check-above warehouse left-coord true)
               [could-move-right new-warehouse] (check-above new-warehouse (vec-plus left-coord [0 1]) false)]
           (if (and could-move-left could-move-right) [true (move-box new-warehouse left-coord dir)] [false warehouse]))
      \v (let [[could-move-left new-warehouse] (check-below warehouse left-coord true)
               [could-move-right new-warehouse] (check-below new-warehouse (vec-plus left-coord [0 1]) false)]
           (if (and could-move-left could-move-right) [true (move-box new-warehouse left-coord dir)] [false warehouse])))))

(defn try-move [current-move warehouse position]
  (let [coords-one-further (vec-plus position (dirs current-move))
        on-one-further (get-2d warehouse coords-one-further)]
    (if (= \# on-one-further) [false warehouse]
        (if (= on-one-further \.) [true (swap-warehouse warehouse position coords-one-further)]
            (let [left-coord-box (if (= \[ on-one-further) coords-one-further (vec-plus coords-one-further [0 -1]))
                  [could-move updated-warehouse] (try-move-box warehouse left-coord-box current-move)]
              (if (not could-move) [false warehouse]
                  [true (swap-warehouse updated-warehouse position coords-one-further)]))))))

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
        _ (println robot-moves)
        robot-position (find-robot warehouse)
        warehouse-end (move-robot robot-moves warehouse robot-position)
        total (score-warehouse warehouse-end)]
    (print-grid warehouse-end)
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
