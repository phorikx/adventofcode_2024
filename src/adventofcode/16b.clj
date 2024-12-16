(ns adventofcode.16b)
(require '[clojure.string :as str])
(require '[clojure.set :as s])

(def dirs {\^ [-1 0]
           \> [0 1]
           \v [1 0]
           \< [0 -1]})
(def left {\^ \<
           \> \^
           \v \>
           \< \v})
(def right {\^ \>
            \> \v
            \v \<
            \< \^})

(defn vec-plus [coord-1 coord-2]
  [(+ (get coord-1 0) (get coord-2 0)) (+ (get coord-1 1) (get coord-2 1))])

(defn vec-times [k coord]
  [(* k (get coord 0)) (* k (get coord 1))])
(defn get-2d [grid coord] (get (get grid (get coord 0)) (get coord 1)))

(defn print-grid [grid] (mapv #(println %) grid))

(defn assoc-2d [grid coord val]
  (assoc grid (get coord 0) (assoc (get grid (get coord 0)) (get coord 1) val)))

(defn make-maze [input]
  (mapv #(into [] (seq (char-array %))) (str/split-lines (get (str/split input #"\n\n") 0))))

(defn find-lowest [already-visited lowest-val lowest-entry]
  (if (= (count already-visited) 0) lowest-entry
      (let [current-key (first (keys already-visited))
            current-val ((get already-visited current-key) :value)
            is_lowest (or (= -1 lowest-val) (< current-val lowest-val))
            new-lowest (if is_lowest current-val lowest-val)
            new-lowest-entry (if is_lowest [current-key (get already-visited current-key)] lowest-entry)]
        (recur (dissoc already-visited current-key) new-lowest new-lowest-entry))))

(defn get-lowest [already-visited]
  (let [first-entry (find-lowest already-visited -1 [-1 -1])
        new-map (dissoc already-visited (get first-entry 0))]
    [first-entry new-map]))

(defn valid-point [point maze lowest-known]
  (and (not (contains? lowest-known point)) (not (= \# (get-2d maze (get point 0))))))

(defn add-to-visited [point value paths already-visited]
  (if (or (= nil (already-visited point)) (<= value ((already-visited point) :value)))
    (let [new-paths-point (mapv #(conj % (get point 0)) paths)]
      (if (= nil (already-visited point))
        (assoc already-visited point {:value value :paths new-paths-point})
        (if (= value ((already-visited point) :value))
          (assoc already-visited point {:value value :paths (into [] (concat new-paths-point ((already-visited point) :paths)))})
          (assoc already-visited point {:value value :paths new-paths-point}))))
    already-visited))

(defn visit-new-sites [point already-visited lowest-known maze]
  (let [current-point (get (get point 0) 0)
        paths ((get point 1) :paths)
        current-dir (get (get point 0) 1)
        current-value ((get point 1) :value)
        p1 (vec-plus current-point (dirs current-dir))
        new-visited (if (valid-point [p1 current-dir] maze lowest-known)
                      (add-to-visited [p1 current-dir] (+ current-value 1) paths already-visited) already-visited)
        new-visited (if (valid-point [current-point (left current-dir)] maze lowest-known)
                      (add-to-visited [current-point (left current-dir)] (+ current-value 1000) paths new-visited) new-visited)
        new-visited (if (valid-point [current-point (right current-dir)] maze lowest-known)
                      (add-to-visited [current-point (right current-dir)] (+ current-value 1000) paths new-visited) new-visited)]
    new-visited))

(defn find-paths-helper [maze lowest-known already-visited start-point end-point value paths]
  (let [[lowest new-visited] (get-lowest already-visited)
        lowest-point-and-dir (get lowest 0)
        lowest-point (get lowest-point-and-dir 0)
        lowest-value ((get lowest 1) :value)
        lowest-paths ((get lowest 1) :paths)
        is-end (= end-point lowest-point)
        too-high (and (> lowest-value value) (not (= 0 value)))
        new-value (if is-end lowest-value value)
        new-paths (if (and is-end (not too-high)) (concat paths lowest-paths) paths)]
    (if too-high paths
        (let [new-known (assoc lowest-known (get lowest 0) (get lowest 1))
              new-visited (visit-new-sites lowest new-visited lowest-known maze)]
          (recur maze new-known new-visited start-point end-point new-value new-paths)))))

(defn find-paths [maze start-point end-point]
  (find-paths-helper maze {} {[start-point \>] {:value 0 :paths [[start-point]]}} start-point end-point 0 []))

(defn find-helper [coords letter maze]
  (if (= letter (get-2d maze (last coords))) (last coords) (recur (butlast coords) letter maze)))

(defn find-start [maze]
  (let [coords (for [i (range (count maze)) j (range (count (get maze 0)))] [i j])]
    (find-helper coords \S maze)))

(defn find-end [maze]
  (let [coords (for [i (range (count maze)) j (range (count (get maze 0)))] [i j])]
    (find-helper coords \E maze)))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_16.txt")
        input (slurp input-file)
        maze (make-maze input)
        start-point (find-start maze)
        end-point (find-end maze)
        paths (find-paths maze start-point end-point)
        points (reduce s/union (mapv #(into #{} %) paths))
        total (count points)]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
