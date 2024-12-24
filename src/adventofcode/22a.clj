(ns adventofcode.22a)
(require '[modules.vectormath :as vec])
(require '[clojure.string :as str])

(defn mix [new-number old-number]
  (bit-xor new-number old-number))

(defn prune [secret-number]
  (mod secret-number 16777216))

(defn calc-first [secret-number]
  (-> secret-number
      (* 64)
      (mix secret-number)
      (prune)))

(defn calc-second [secret-number]
  (-> secret-number
      (quot 32)
      (mix secret-number)
      (prune)))

(defn calc-third [secret-number]
  (-> secret-number
      (* 2048)
      (mix secret-number)
      (prune)))

(defn calculate-next-number [secret-number]
  (let [after-first (calc-first secret-number)
        after-second (calc-second after-first)
        after-third (calc-third after-second)] after-third))

(defn applied-n-times [secret-number n]
  ((apply comp (repeat n calculate-next-number)) secret-number))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_22.txt")
        input (slurp input-file)
        secret-numbers (mapv Integer/parseInt (str/split-lines input))
        results (mapv #(applied-n-times % 2000) secret-numbers)
        total (reduce + results)]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
