(ns adventofcode.14a)
(require '[clojure.string :as str])

(def max-x 101)
(def max-y 103)
(def times-to-move 100)

(defn vec-plus [coord-1 coord-2]
  [(+ (get coord-1 0) (get coord-2 0)) (+ (get coord-1 1) (get coord-2 1))])

(defn vec-times [k coord]
  [(* k (get coord 0)) (* k (get coord 1))])
(defn get-2d [grid coord] (get (get grid (get coord 0)) (get coord 1)))

(defn wrap-position [coord] [(mod (get coord 0) max-x) (mod (get coord 1) max-y)])

(defn make-robots [input]
  (let [robots (str/split-lines input)
        robots (mapv (fn [line] (re-matches #"[^\d]+(\d+),(\d+)[^\d\-]+([\-\d]+),([\-\d]+)" line)) robots)
        robots (mapv #(subvec % 1 5) robots)
        robots (mapv #(mapv Integer/parseInt %) robots)
        robots (mapv (fn [robot] {:start (subvec robot 0 2) :velocity (subvec robot 2 4)}) robots)]
    robots))

(defn move-robot [robot]
  (wrap-position (vec-plus (robot :start) (vec-times times-to-move (robot :velocity)))))

(defn calc-solution [quart moved-robots]
  (let [max-x (if (= 0 (get quart 0)) (- (quot max-x 2) 1) max-x)
        max-y (if (= 0 (get quart 1)) (- (quot max-y 2) 1) max-y)
        min-x (if (= 0 (get quart 0)) 0 (+ (quot max-x 2) 1))
        min-y (if (= 0 (get quart 1)) 0 (+ (quot max-y 2) 1))
        valid-robots (filter (fn [robot]
                               (and (and (and
                                          (>= max-x (get robot 0))
                                          (<= min-x (get robot 0)))
                                         (>= max-y (get robot 1)))
                                    (<= min-y (get robot 1)))) moved-robots)]
    (count valid-robots)))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_14.txt")
        input (slurp input-file)
        robots (make-robots input)
        moved-robots (mapv move-robot robots)
        solution-per-quart (mapv #(calc-solution % moved-robots) (for [i (range 2) j (range 2)] [i j]))
        total (reduce * solution-per-quart)]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
