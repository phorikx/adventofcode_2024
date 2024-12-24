(ns adventofcode.23a
  (:require
   [clojure.string :as str]))
(require '[clojure.set :as set])

(defn add-to-map [m k v]
  (if (= nil (m k)) (assoc m k [v])
      (assoc m k (conj (m k) v))))

(defn build-connections-helper [input map-so-far]
  (if (= 0 (count input)) map-so-far
      (let [connection (first input)
            new-map (add-to-map map-so-far (get connection 0) (get connection 1))
            new-map (add-to-map new-map (get connection 1) (get connection 0))]
        (recur (rest input) new-map))))

(defn build-connections [input]
  (build-connections-helper input {}))

(defn find-specific-group [connections to-check valid-targets computer groups]
  (if (= 0 (count to-check)) groups
      (let [overlap (set/intersection valid-targets (into #{} (connections (first to-check))))
            valid-groups (for [i overlap] [computer (first to-check) i])
            new-groups (concat groups valid-groups)]
        (recur connections (rest to-check) valid-targets computer new-groups))))

(defn find-groups [computer connections]
  (let [valid-targets (connections computer)
        groups (find-specific-group connections valid-targets (into #{} valid-targets) computer [])] groups))

(defn remove-computer-helper [to-go connections computer]
  (if (= 0 (count to-go)) connections
      (let [k (first to-go)
            new-coll (filter #(not (= computer %)) (connections k))
            connections (assoc connections k new-coll)] connections)))

(defn remove-computer [connections computer]
  (let [connections (dissoc connections computer)
        connections (remove-computer-helper (keys connections) connections computer)]
    connections))

(defn find-groups-of-three-helper [computers connections groups]
  (if (= 0 (count computers)) groups
      (let [groups-for-this (find-groups (first computers) connections)
            new-groups (concat groups groups-for-this)
            new-connections (remove-computer connections (first computers))]
        (recur (rest computers) new-connections new-groups))))

(defn find-groups-of-three [connections]
  (find-groups-of-three-helper (keys connections) connections []))

(defn find-ts [groups]
  (filter (fn [group] (> (count (filter #(str/starts-with? % "t") group)) 0)) groups))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_23.txt")
        input (mapv #(str/split % #"-") (str/split-lines (slurp input-file)))
        connections (build-connections  input)
        groups-of-three (find-groups-of-three connections)
        sets-of-three (into #{} (mapv #(into #{} %) groups-of-three))
        groups-with-t (find-ts sets-of-three)
        total (count groups-with-t)]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
