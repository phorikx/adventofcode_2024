(ns adventofcode.20a
  (:require
   [modules.vectormath :as vec]))
(require '[clojure.string :as str])
(require '[modules.vectormath :as vec])

(def threshold 100)

(defn make-maze [input]
  (mapv #(into [] (seq (char-array %))) (str/split-lines input)))

(defn valid-neighbor [maze current previous]
  (let [neighbors (vec/neighbors current)
        neighbors (filter #(not (= previous %)) neighbors)
        neighbors (filter #(not (= \# (vec/get-2d maze %))) neighbors)]
    (first neighbors)))

(defn find-path-helper [maze path current previous path-length]
  (let [next-one (valid-neighbor maze current previous)
        path (assoc path current path-length)]
    (if (= \E (vec/get-2d maze current)) path
        (recur maze path next-one current (+ 1 path-length)))))

(defn find-path [maze start-point]
  (find-path-helper maze {} start-point nil 0))

(defn find-helper [coords letter maze]
  (if (= letter (vec/get-2d maze (last coords))) (last coords) (recur (butlast coords) letter maze)))

(defn find-start [maze]
  (let [coords (for [i (range (count maze)) j (range (count (get maze 0)))] [i j])]
    (find-helper coords \S maze)))

(defn shortcut-location-helper [path loc distance neighbors total]
  (if (= 0 (count neighbors)) total
      (let [new-distance (+ 2 distance)
            old-distance (path (last neighbors))
            new-total (if (= nil old-distance) total (if (>= (- old-distance new-distance) threshold) (+ total 1) total))]
        (recur path loc distance (butlast neighbors) new-total))))

(defn count-shortcuts-helper [path remaining total]
  (if (= 0 (count remaining)) total
      (let [current-loc (get (last remaining) 0)
            current-distance (get (last remaining) 1)
            neighbors (vec/neighbors-2 current-loc)
            shortcuts-for-location (shortcut-location-helper path current-loc current-distance neighbors 0)]
        (recur path (butlast remaining) (+ shortcuts-for-location total)))))

(defn count-shortcuts [path]
  (count-shortcuts-helper path path 0))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_20.txt")
        input (slurp input-file)
        maze (make-maze input)
        start-point (find-start maze)
        path (find-path maze start-point)
        shortcuts (count-shortcuts path)]
    (println shortcuts)))

(when (= *command-line-args* ["run"])
  (-main))
