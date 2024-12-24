(ns adventofcode.24a)
(require '[clojure.string :as str])
(require '[clojure.math :as math])
(require '[clojure.set :as set])

(def op {"OR" (fn [op1 op2] (if (or (= 1 op1) (= 1 op2)) 1 0))
         "AND" (fn [op1 op2] (if (and (= 1 op1) (= 1 op2)) 1 0))
         "XOR" (fn [op1 op2] (if (not (= op1 op2)) 1 0))})

(defn make-inputs [input]
  (let [lines (str/split-lines input)
        array (mapv #(into [] (str/split % #": ")) lines)
        to-int (mapv #(assoc % 1 (Integer/parseInt (get % 1))) array)] (into {} to-int)))

(defn make-function [line]
  (let [[before after] (into [] (str/split line #" -> "))
        [operand1 operation operand2] (into [] (str/split before #" "))]
    (fn [environment]
      (if (or (= nil (environment operand1)) (= nil (environment operand2))) environment
          (assoc environment after ((op operation) (environment operand1) (environment operand2)))))))

(defn make-gates [lines]
  (let [lines (str/split-lines lines)
        functions (mapv make-function lines)]
    functions))

(defn apply-all-gates [gates inputs]
  (let [funs (apply comp gates)
        new-output (funs inputs)]
    new-output))

(defn apply-gates [gates inputs]
  (let [new-inputs (apply-all-gates gates inputs)]
    (if (= new-inputs inputs) inputs
        (recur gates new-inputs))))

(defn calc-sum-helper [sorted total it applied]
  (if (= 0 (count sorted)) total
      (recur (rest sorted) (+ (bigint total) (bigint (* (applied (first sorted)) (bigint (math/pow 2 it))))) (+ 1 it) applied)))

(defn calc-output [applied]
  (let [zs (filter #(str/starts-with? % "z") (keys applied))
        sorted (sort zs)
        sum (calc-sum-helper sorted (bigint 0) 0 applied)]
    sum))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_24.txt")
        [input gates] (str/split (slurp input-file) #"\n\n")
        inputs (make-inputs input)
        gates (make-gates gates)
        applied (apply-gates gates inputs)
        output (calc-output applied)]
    (println output)
    (println applied)))

(when (= *command-line-args* ["run"])
  (-main))
