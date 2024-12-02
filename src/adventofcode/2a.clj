(ns adventofcode.2a)
(require '[clojure.string :as str])

(defn parse-row [row]
  (mapv Integer/parseInt (str/split row #"\s+")))

(defn build-reports [input]
  (mapv  parse-row (str/split-lines input)))

(defn order-reports [input]
  (mapv #(if (> (last %) (first %)) % (into [] (rseq %))) input))

(defn seq_okay [vec]
  (if (= 1 (count vec)) true (and (and (<= 1 (- (last vec) (last (butlast vec)))) (>= 3 (- (last vec) (last (butlast vec))))) (recur (butlast vec)))))

(defn sum [ordered-reports]
  (count (filter identity ordered-reports)))

(defn total-okay [reports]
  (sum (mapv seq_okay reports)))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_2.txt")
        input (slurp input-file)
        reports (build-reports input)
        ordered-reports (order-reports reports)
        total (total-okay ordered-reports)]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
