(ns adventofcode.9a)
(require '[clojure.string :as str])

(defn free-spaces [file-system])

(defn files [file-system])

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_9.txt")
        input (slurp input-file)
        into_chars (seq (char-array (str/trim input)))
        into_chars (if (= 1 (mod (count into_chars) 2)) into_chars (butlast into_chars))
        file-system (mapv #(Character/digit % 10) into_chars)
        total (count-recursively file-system 0 (bigint 0) false 0 end_index_counter [] [])]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
