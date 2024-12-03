(ns adventofcode.3a)
(require '[clojure.string :as str])

(defn get-matches [input]
  (re-seq #"mul\((\d+),(\d+)\)" input))

(defn calculate-mults [matches]
  (mapv #(* (Integer/parseInt (get % 1)) (Integer/parseInt (get % 2))) matches))

(defn calc-total [mults]
  (reduce + mults))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_3.txt")
        input (slurp input-file)
        matches (get-matches input)
        mults (calculate-mults matches)
        total (calc-total mults)]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
