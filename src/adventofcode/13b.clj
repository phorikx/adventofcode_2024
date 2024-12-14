(ns adventofcode.13b)
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
        machines (mapv (fn [machine] {\A (mapv bigint (get machine 0))
                                      \B (mapv bigint (get machine 1))
                                      \G (mapv #(+ (bigint %) 10000000000000) (get machine 2))}) machines)]
    machines))

(defn gcd [num1 num2] (if (zero? num2) num1 (recur num2 (mod num1 num2))))

(defn find-solution-helper [machine a b total]
  (let [a1 (get (machine \A) 0)
        a2 (get (machine \A) 1)
        b1 (get (machine \B) 0)
        b2 (get (machine \B) 1)
        g1 (get (machine \G) 0)
        g2 (get (machine \G) 1)
        y (/ (- g1 (* (/ a1 a2) g2)) (- b1 (* (/ a1 a2) b2)))
        x (/ (- g1 (* (/ b1 b2) g2)) (- a1 (* (/ b1 b2) a2)))]
    (if (and (integer? y) (integer? x)) (+ (* x 3) y) 0)))

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
