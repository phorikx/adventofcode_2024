(ns adventofcode.11a)
(require '[clojure.string :as str])

(defn create-stones [input]
  (mapv bigint (str/split (str/trim input) #" ")))

(def apply-rule-to-stone
  (memoize
   (fn [stone]
     (if (= 0 stone) [(bigint 1)]
         (let [no_of_digits (count (str stone))
               is_even? (= 0 (mod no_of_digits 2))]
           (if is_even?
             (let [as_str (str stone)
                   halfway_point (/ no_of_digits 2)
                   first_half (bigint (subs as_str 0 halfway_point))
                   second_half (bigint (subs as_str halfway_point no_of_digits))]
               [(bigint first_half) (bigint second_half)])
             [(* 2024 stone)]))))))

(def count-stones
  (memoize
   (fn [stone counter]
     (if (= 0 counter) 1
         (let [new-stones (apply-rule-to-stone stone)]
           (if (= 1 (count new-stones)) (count-stones (get new-stones 0) (- counter 1))
               (reduce + (mapv #(count-stones % (- counter 1)) new-stones))))))))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_11.txt")
        input (slurp input-file)
        stones (create-stones input)
        total (reduce + (mapv #(count-stones % 25) stones))]
    (println stones)
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
