(ns adventofcode.2b)
(require '[clojure.string :as str])

(defn parse-row [row]
  (mapv Integer/parseInt (str/split row #"\s+")))

(defn build-reports [input]
  (mapv  parse-row (str/split-lines input)))

(defn order-reports [input]
  (mapv #(if (> (last %) (first %)) % (into [] (rseq %))) input))

(defn sum [ordered-reports]
  (count (filter identity ordered-reports)))

(defn seq_okay_test [vec]
  (if (= 1 (count vec)) true (and (and (<= 1 (- (last vec) (last (butlast vec)))) (>= 3 (- (last vec) (last (butlast vec))))) (recur (butlast vec)))))

(defn seq_okay [vec]
  (let [seq_okay_val (seq_okay_test vec)]
    (when seq_okay_val (println vec)) seq_okay_val))

(defn sans-n [vec n]
  (into [] (concat (subvec vec 0 n) (subvec vec (+ n 1) (count vec)))))

(defn test-sequences [reports]
  (some identity (mapv #(seq_okay (sans-n reports %)) (range 0 (count reports)))))

(defn total-okay [reports]
  (sum (mapv #(if (seq_okay %) true (test-sequences %)) reports)))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_2.txt")
        input (slurp input-file)
        reports (build-reports input)
        ordered-reports (order-reports reports)
        total (total-okay ordered-reports)]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
