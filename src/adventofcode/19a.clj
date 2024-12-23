(ns adventofcode.19a)
(require '[clojure.string :as str])
(declare is-possible)
(declare is-possible-helper)

(defn parse-input [input]
  (let [[first-part second-part] (into [] (str/split input #"\n\n"))
        towels (str/split first-part #", ")
        patterns (str/split-lines second-part)]
    [towels patterns]))

(def is-possible-helper (fn [pattern to-check towels]
                          (if (= 0 (count to-check)) false
                              (let [current-towel (last to-check)
                                    possible (str/starts-with? pattern current-towel)
                                    possible (if (not possible) false (is-possible (str/replace-first pattern current-towel "") towels))]
                                (if possible true
                                    (recur pattern (butlast to-check) towels))))))

(def is-possible (memoize (fn [pattern towels]
                            (if (= 0 (count pattern)) true
                                (is-possible-helper pattern towels towels)))))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_19.txt")
        input (slurp input-file)
        [towels patterns] (parse-input input)
        possible (filter #(is-possible % towels) patterns)
        total (count possible)]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
