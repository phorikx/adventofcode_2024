(ns adventofcode.19b)
(require '[clojure.string :as str])
(declare possible-arrangements)
(declare is-possible-helper)

(defn parse-input [input]
  (let [[first-part second-part] (into [] (str/split input #"\n\n"))
        towels (str/split first-part #", ")
        patterns (str/split-lines second-part)]
    [towels patterns]))

(def is-possible-helper (fn [pattern to-check towels total]
                          (if (= 0 (count to-check)) total
                              (let [current-towel (last to-check)
                                    possible (str/starts-with? pattern current-towel)
                                    sub-possibilities (if (not possible) 0
                                                          (possible-arrangements (str/replace-first pattern current-towel "") towels))
                                    new-total (bigint (if possible (+ total sub-possibilities) total))]
                                (recur pattern (butlast to-check) towels new-total)))))

(def possible-arrangements (memoize (fn [pattern towels]
                                      (if (= 0 (count pattern)) (bigint 1)
                                          (is-possible-helper pattern towels towels (bigint 0))))))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_19.txt")
        input (slurp input-file)
        [towels patterns] (parse-input input)
        possible (mapv #(possible-arrangements % towels) patterns)
        total (reduce + possible)]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
