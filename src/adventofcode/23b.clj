(ns adventofcode.23b)
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(declare find-longest-specific-group-helper)

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

(defn find-longest-specific-group-helper [current-group possible-additions connections longest-current]
  (if (= 0 (count possible-additions)) longest-current
      (let [adding (first possible-additions)
            could-fit (set/subset? (into #{} current-group) (into #{} (connections adding)))
            longest-with-adding (if (not could-fit) current-group
                                    (find-longest-specific-group-helper
                                     (conj current-group adding)
                                     (set/intersection (into #{} possible-additions) (into #{} (connections adding)))
                                     connections
                                     (conj current-group adding)))
            new-longest (if (> (count longest-with-adding) (count longest-current)) longest-with-adding longest-current)]
        (recur current-group (rest possible-additions) connections new-longest))))

(defn find-longest-specific-group [computer connections]
  (find-longest-specific-group-helper (vector computer) (into #{} (connections computer)) connections (vector computer)))

(defn remove-computer-helper [to-go connections computer]
  (if (= 0 (count to-go)) connections
      (let [k (first to-go)
            new-coll (filter #(not (= computer %)) (connections k))
            connections (assoc connections k new-coll)] connections)))

(defn remove-computer [connections computer]
  (let [connections (dissoc connections computer)
        connections (remove-computer-helper (keys connections) connections computer)]
    connections))

(defn find-longest-group-helper [computers connections longest-group]
  (if (= 0 (count computers)) longest-group
      (let [longest-groups-for-this (find-longest-specific-group (first computers) connections)
            new-longest-group (if (> (count longest-groups-for-this) (count longest-group)) longest-groups-for-this longest-group)
            new-connections (remove-computer connections (first computers))]
        (recur (rest computers) new-connections new-longest-group))))

(defn find-longest-group [connections]
  (find-longest-group-helper (keys connections) connections []))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_23.txt")
        input (mapv #(str/split % #"-") (str/split-lines (slurp input-file)))
        connections (build-connections  input)
        longest-group (find-longest-group connections)
        sorted (sort longest-group)
        result (str/join "," sorted)]
    (println longest-group)
    (println sorted)
    (println result)))

(when (= *command-line-args* ["run"])
  (-main))
