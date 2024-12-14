(ns adventofcode.13a)
(require '[clojure.string :as str])

(defn vec-plus [coord-1 coord-2]
  [(+ (get coord-1 0) (get coord-2 0)) (+ (get coord-1 1) (get coord-2 1))])

(defn get-2d [grid coord] (get (get grid (get coord 0)) (get coord 1)))

(defn make-machines [input]
  (let [machines (str/split (str/trim input) #"\n\n")
        machines (mapv str/split-lines machines)
        machines (mapv
                  (fn [machine] [(subvec (re-matches #"[^\d]+(\d+)[^\d]+(\d+)" (get machine 0)) 1 3)
                                 (subvec (re-matches #"[^\d]+(\d+)[^\d]+(\d+)" (get machine 1)) 1 3)
                                 (subvec (re-matches #"[^\d]+(\d+)[^\d]+(\d+)" (get machine 2)) 1 3)]) machines)
        machines (mapv (fn [machine] {\A (mapv Integer/parseInt (get machine 0))
                                      \B (mapv Integer/parseInt (get machine 1))
                                      \G (mapv Integer/parseInt (get machine 2))}) machines)]
    machines))

(defn find-solution-helper [machine a b total]
  (if (and (= 100 a) (= 100 b)) total
      (let [calc [(+ (* a (get (machine \A) 0)) (* b (get (machine \B) 0)))
                  (+ (* a (get (machine \A) 1)) (* b (get (machine \B) 1)))]
            equal? (= (machine \G) calc)
            new-a (if (= b 100) (+ 1 a) a)
            new-b (if (= b 100) 0 (+ 1 b))
            total-for-now (+ (* 3 a) b)
            new-total (if equal? (if (= 0 total) total-for-now (min total-for-now total)) total)]
        (recur machine new-a new-b new-total))))

(defn find-solution [machine]
  (find-solution-helper machine 0 0 0))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_13.txt")
        input (slurp input-file)
        machines (make-machines input)
        solutions (mapv find-solution machines)
        total (reduce + solutions)]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
