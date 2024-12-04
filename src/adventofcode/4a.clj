(ns adventofcode.4a)
(require '[clojure.string :as str])

(defn char-count [string c]
  (count (filter #(= c %) (char-array string))))

(defn power-generator [i j]
  (cond
    (and (< 0 i) (< 0 j)) 1
    (and (> 0 i) (< 0 j)) 10
    (and (= 0 i) (< 0 j)) 100
    (and (< 0 i) (> 0 j)) 1000
    (and (> 0 i) (> 0 j)) 10000
    (and (= 0 i) (> 0 j)) 100000
    (and (< 0 i) (= 0 j)) 1000000
    (and (> 0 i) (= 0 j)) 10000000))

(defn letter-generator [i j]
  (cond
    (= 1 (max (abs i) (abs j))) \M
    (= 2 (max (abs i) (abs j))) \A
    (= 3 (max (abs i) (abs j))) \S))

(defn matrix-generator [i j]
  (if (and (= 0 i) (= 0 j)) (fn [char] 0)
      (fn [char]
        (if (= nil char) 0
            (if (= (letter-generator i j) char) (power-generator i j) 0)))))

(defn apply-matrix [coords arr]
  (let [[x y] coords
        conv-matrix (mapv (fn [i] (mapv (fn [j] ((matrix-generator i j) (get (get arr (+ j y)) (+ i x)))) (range -3 4))) (range -3 4))]
    (println conv-matrix)
    (reduce + (reduce concat conv-matrix))))

(defn count-christmas [coords arr]
  (let [result (apply-matrix coords arr)
        str-num (str result)]
    (println "Counting Christmas")
    (char-count str-num \3)))

(defn count-words [coords arr]
  (let [[i j] coords
        char (get (get arr j) i)]
    (if (= \X char) (count-christmas coords arr) 0)))

(defn search-word [arr]
  (let [words-found (mapv (fn [i] (mapv (fn [j] (count-words [i j] arr)) (range (count (first arr))))) (range (count arr)))]
    (reduce + (reduce concat words-found))))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_4.txt")
        input (slurp input-file)
        char-vec (mapv  #(into [] (seq (char-array %))) (str/split input #"\n"))
        total (search-word char-vec)]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
