(ns day1.1b)
(require '[clojure.string :as str])

(defn build-lists [file]
  (let [lines (str/split-lines file)
        split-lines (mapv #(str/split % #"\s+") lines)
        list1 (mapv #(Integer/parseInt (get % 0)) split-lines)
        list2 (mapv #(Integer/parseInt (get % 1)) split-lines)]
    (vector list1 list2)))

(defn get-answer [list1 list2 total]
  (if (empty? list1) total
      (recur (vec (butlast list1)) list2 (+ total (* (last list1) (count (filter #(= (last list1) %) list2)))))))

(defn run []
  (let [input (slurp "./src/day1/input.txt")
        lists (build-lists input)
        sorted-lists (mapv #(sort %) lists)
        [list1 list2] sorted-lists
        answer (get-answer list1 list2 0)]
    (println answer)))

(run)
