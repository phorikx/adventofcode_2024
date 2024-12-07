(ns adventofcode.7a)
(require '[clojure.string :as str])
(require '[clojure.math :as math])

(def operations [+ *])
(defn tail [vec] (into [] (subvec vec 1 (count vec))))

(defn parse-input [input]
  (let [split-lines (mapv #(str/split % #":") (str/split-lines input))
        split-operations (mapv #(assoc % 1 (str/split (str/trim (get % 1)) #" ")) split-lines)
        to-ints (mapv #(vector (BigInteger. (get % 0)) (mapv (fn [x] (Integer/parseInt x)) (get % 1))) split-operations)]
    to-ints))

(defn binary-expension [power n]
  (int (mod (quot power (math/pow 2 n)) 2)))

(defn generate-function [parsed-input power]
  (apply comp (reverse (for [x (range (count parsed-input))]
                         (fn [y]
                           ((get operations (binary-expension power x))
                            y (get parsed-input x)))))))

(defn generate-combinations [function-numbers]
  (mapv #(generate-function function-numbers %)
        (range (math/pow 2 (count function-numbers)))))

(defn to-funcs [parsed-input]
  (mapv #(vector (get % 0) (get (get % 1) 0) (generate-combinations (tail (get % 1)))) parsed-input))

(defn any-result [row]
  (let [results (mapv #(% (get row 1)) (get row 2))]
    (reduce #(or %1 %2) (mapv #(= (get row 0) %1) results))))

(defn check-results [combinations]
  (mapv #(if (any-result %) (get % 0) 0) combinations))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_7.txt")
        input (slurp input-file)
        parsed-input (parse-input input)
        combinations (to-funcs parsed-input)
        results (check-results combinations)
        total (reduce + results)]
    (println results)
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
