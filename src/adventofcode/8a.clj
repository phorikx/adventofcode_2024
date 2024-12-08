(ns adventofcode.8a)
(require '[clojure.string :as str])
(require '[clojure.math :as math])
(require '[clojure.set :refer [union]])

(defn print-grid [grid] (mapv #(println %) grid))
(defn vec-plus [coord-1 coord-2]
  [(+ (get coord-1 0) (get coord-2 0)) (+ (get coord-1 1) (get coord-2 1))])

(defn vec-minus [coord-1 coord-2]
  [(- (get coord-1 0) (get coord-2 0)) (- (get coord-1 1) (get coord-2 1))])

(defn get-2d [grid coord] (get (get grid (get coord 0)) (get coord 1)))

(defn make-grid [input-file]
  (into [] (mapv #(into [] (seq (char-array %))) (str/split-lines input-file))))

(defn get-set [points antenna]
  (if (not (= nil (points antenna))) (points antenna) #{}))

(defn combine-map [coord sym points]
  (assoc points sym (conj (get-set points sym) coord)))

(defn make-new-map [points coord grid]
  (if (= \. (get-2d grid coord)) points
      (combine-map coord (get-2d grid coord) points)))

(defn make-map [coords points grid]
  (if (= 0 (count coords)) points
      (let [new-map (make-new-map points (last coords) grid)]
        (recur (butlast coords) new-map grid))))

(defn parse-input [grid]
  (make-map (for [i (range (count grid)) j (range (count (get grid 0)))]
              [i j]) {} grid))

(defn in-grid? [coord size]
  (and (and (<= 0 (get coord 0)) (> (get size 0) (get coord 0))) (and (<= 0 (get coord 1)) (> (get size 1) (get coord 1)))))

(defn anti-nodes-for-two [i j]
  #{(vec-plus i (vec-minus i j)) (vec-plus j (vec-minus j i))})

(defn make-anti-nodes [current-nodes anti-nodes]
  (if (= 0 (count current-nodes)) anti-nodes
      (let [current-node (last current-nodes)
            i (get current-node 0)
            j (get current-node 1)
            new-anti-nodes
            (if (= i j) anti-nodes
                (union anti-nodes (anti-nodes-for-two i j)))]
        (recur (butlast current-nodes) new-anti-nodes))))

(defn anti-node-per-antenna [nodes]
  (make-anti-nodes (for [i nodes j nodes] [i j]) #{}))

(defn calculate-anti-nodes [nodes]
  (let [anti-nodes (mapv anti-node-per-antenna (vals nodes))
        total-anti-nodes (reduce union anti-nodes)]
    (println anti-nodes)
    total-anti-nodes))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_8.txt")
        input (slurp input-file)
        grid (make-grid input)
        nodes (parse-input grid)
        anti-nodes (calculate-anti-nodes nodes)
        size [(count grid) (count (get grid 0))]
        valid-anti-nodes (into #{} (filter #(in-grid? % size) anti-nodes))
        total (count valid-anti-nodes)]
    (println nodes)
    (println valid-anti-nodes)
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
