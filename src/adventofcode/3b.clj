(ns adventofcode.3b)
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
        split-input (str/split input #"do\(\)")
        further-split-input (mapv #(str/split % #"don\'t\(\)") split-input)
        only-take-relevant (mapv #(get % 0) further-split-input)
        modified-input (reduce str only-take-relevant)
        matches (get-matches modified-input)
        mults (calculate-mults matches)
        total (calc-total mults)]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
