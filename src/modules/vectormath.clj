(ns modules.vectormath)
(declare vec-plus)
(declare vec-times)
(declare get-2d)
(declare assoc-2d)
(declare print-grid)
(declare neighbors)

(defn neighbors [location]
  [(vec-plus location [0 1]) (vec-plus location [0 -1]) (vec-plus location [1 0]) (vec-plus location [-1 0])])

(defn neighbors-2 [location]
  [(vec-plus location [1 1]) (vec-plus location [1 -1]) (vec-plus location [-1 1]) (vec-plus location [-1 -1])
   (vec-plus location [0 2]) (vec-plus location [0 -2]) (vec-plus location [2 0]) (vec-plus location [-2 0])])

(defn neighbors-n [location n]
  (for [i (range (- 0 n) (+ 1 n)) j (range (- 0 n) (+ 1 n)) :when (>= n (+ (abs i) (abs j)))] (vec-plus location [i j])))

(defn vec-plus [coord-1 coord-2]
  [(+ (get coord-1 0) (get coord-2 0)) (+ (get coord-1 1) (get coord-2 1))])

(defn vec-dif [coord-1 coord-2]
  [(- (get coord-1 0) (get coord-2 0)) (- (get coord-1 1) (get coord-2 1))])

(defn vec-times [k coord]
  [(* k (get coord 0)) (* k (get coord 1))])
(defn get-2d [grid coord] (get (get grid (get coord 0)) (get coord 1)))

(defn print-grid [grid] (mapv #(println %) grid))

(defn assoc-2d [grid coord val]
  (assoc grid (get coord 0) (assoc (get grid (get coord 0)) (get coord 1) val)))
