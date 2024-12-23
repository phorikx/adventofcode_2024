(ns adventofcode.17a)
(require '[clojure.string :as str])
(require '[clojure.math :as math])

(defn literal-operand [environment instruction-pointer]
  (get (environment :tape) (+ 1 instruction-pointer)))

(defn combo-operand [environment instruction-pointer]
  (let [literal (literal-operand environment instruction-pointer)]
    (if (< literal 4) literal
        (case literal
          4 (environment :A)
          5 (environment :B)
          6 (environment :C)))))

(def functions {0 (fn [environment instruction-pointer]
                    (let [denom (environment :A)
                          operand (combo-operand environment instruction-pointer)
                          dividor (int (math/pow 2 operand))
                          value (quot denom dividor)
                          environment (assoc environment :A value)]
                      [environment instruction-pointer nil false]))
                1 (fn [environment instruction-pointer]
                    (let [b (environment :B)
                          other (literal-operand environment instruction-pointer)
                          answer (bit-xor (int b) other)
                          environment (assoc environment :B answer)]
                      [environment instruction-pointer nil false]))
                2 (fn [environment instruction-pointer]
                    (let [environment (assoc environment :B (mod (combo-operand environment instruction-pointer) 8))]
                      [environment instruction-pointer nil false]))
                3 (fn [environment instruction-pointer]
                    (if (= 0 (environment :A))
                      [environment instruction-pointer nil false]
                      [environment (literal-operand environment instruction-pointer) nil true]))
                4 (fn [environment instruction-pointer]
                    (let [b (environment :B)
                          c (environment :C)
                          answer (bit-xor (int b) (int c))
                          environment (assoc environment :B answer)]
                      [environment instruction-pointer nil false]))
                5 (fn [environment instruction-pointer]
                    (let [to-out (mod (combo-operand environment instruction-pointer) 8)]
                      [environment instruction-pointer to-out false]))
                6 (fn [environment instruction-pointer]
                    (let [denom (environment :A)
                          operand (combo-operand environment instruction-pointer)
                          dividor (int (math/pow 2 operand))
                          value (quot denom dividor)
                          environment (assoc environment :B value)]
                      [environment instruction-pointer nil false]))
                7 (fn [environment instruction-pointer]
                    (let [denom (environment :A)
                          operand (combo-operand environment instruction-pointer)
                          dividor (int (math/pow 2 operand))
                          value (quot denom dividor)
                          environment (assoc environment :C value)]
                      [environment instruction-pointer nil false]))})

(defn make-environment [input]
  (let [lines (str/split-lines input)
        a (bigint (get (str/split (get lines 0) #": ") 1))
        b (Integer/parseInt (get (str/split (get lines 1) #": ") 1))
        c (Integer/parseInt (get (str/split (get lines 2) #": ") 1))
        tape (mapv Integer/parseInt (str/split (get (str/split (get lines 4) #": ") 1) #","))]
    {:A a
     :B b
     :C c
     :tape tape}))

(defn follow-tape [environment instruction-pointer out-vec]
  (let [function (functions (get (environment :tape) instruction-pointer))
        [environment instruction-pointer out edited-instruction-pointer] (function environment instruction-pointer)
        out-vec (if (= nil out) out-vec (conj out-vec out))
        new-instruction-pointer (if edited-instruction-pointer instruction-pointer (+ instruction-pointer 2))]
    (if (= nil (get (environment :tape) new-instruction-pointer)) out-vec
        (recur environment new-instruction-pointer out-vec))))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_17.txt")
        input (slurp input-file)
        environment (make-environment input)
        out (follow-tape environment 0 [])
        out-str (str/join "," out)]
    (println out-str)))

(when (= *command-line-args* ["run"])
  (-main))
