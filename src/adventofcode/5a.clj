(ns adventofcode.5a)
(require '[clojure.string :as str])

(defn first-index [coll el]
  (loop [arr coll n 0]
    (if (= 0 (count arr)) nil
        (if (= el (first arr))
          n
          (recur (next arr) (inc n))))))

(defn create-rule [line]
  (let [pages (str/split line #"\|")]
    (fn [coll] (let [index-first (first-index coll (get pages 0))
                     index-second (first-index coll (get pages 1))]
                 (if (or (= nil index-first) (= nil index-second)) true
                     (< index-first index-second))))))

(defn create-rules [rules-string]
  (let [lines (str/split-lines rules-string)]
    (mapv create-rule lines)))

(defn create-colls [input]
  (mapv #(str/split % #",") (str/split-lines input)))

(defn apply-rules [coll rules]
  (let [results (mapv #(% coll) rules)]
    (reduce #(and %1 %2) results)))

(defn get-middle [coll]
  (let [mid-index (- (/ (count coll) 2) (/ 1 2))
        mid-value (get coll mid-index)]
    (Integer/parseInt mid-value)))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_5.txt")
        input (slurp input-file)
        split-inputs (str/split input #"\n\n")
        rules (create-rules (get split-inputs 0))
        colls (create-colls (get split-inputs 1))
        valid-colls (filter #(apply-rules % rules) colls)
        middles (mapv get-middle valid-colls)
        sum (reduce + middles)]
    (println sum)))

(when (= *command-line-args* ["run"])
  (-main))
