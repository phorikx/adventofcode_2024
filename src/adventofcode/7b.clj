(ns adventofcode.7b)
(require '[clojure.string :as str])
(require '[clojure.math :as math])

(defn concatenate [x y]
  (BigInteger. (apply str (vector x y))))

(def operations [+ * concatenate])
(defn tail [vec] (into [] (subvec vec 1 (count vec))))

(defn parse-input [input]
  (let [split-lines (mapv #(str/split % #":") (str/split-lines input))
        split-operations (mapv #(assoc % 1 (str/split (str/trim (get % 1)) #" ")) split-lines)
        to-ints (mapv #(vector (BigInteger. (get % 0)) (mapv (fn [x] (BigInteger. x)) (get % 1))) split-operations)]
    to-ints))

(defn ternary-expension [power n]
  (int (mod (quot power (math/pow 3 n)) 3)))

(defn generate-function [parsed-input power]
  (apply comp (reverse (for [x (range (count parsed-input))]
                         (fn [y]
                           ((get operations (ternary-expension power x))
                            y (get parsed-input x)))))))

(defn generate-combinations [function-numbers]
  (mapv #(generate-function function-numbers %)
        (range (math/pow 3 (count function-numbers)))))

(defn to-funcs [row]
  (vector (get row 0) (get (get row 1) 0) (generate-combinations (tail (get row 1)))))

(defn any-result [row]
  (let [results (mapv #(% (get row 1)) (get row 2))]
    (reduce #(or %1 %2) (mapv #(= (get row 0) %1) results))))

(defn make-solution [row]
  (let [func-row (to-funcs row)
        result (if (any-result func-row) (get func-row 0) 0)]
    result))

(defn make-solutions [input]
  (mapv make-solution input))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_7.txt")
        input (slurp input-file)
        parsed-input (parse-input input)
        results (make-solutions parsed-input)
        total (reduce + results)]
    (println results)
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
