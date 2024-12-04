(ns adventofcode.4b)
(require '[clojure.string :as str])

(defn char-count [string c]
  (count (filter #(= c %) (char-array string))))

(defn power-generator [i j]
  (if (= (abs i) (abs j))
    (cond
      (and (< 0 i) (< 0 j)) 1
      (and (> 0 i) (< 0 j)) 10
      (and (< 0 i) (> 0 j)) 10
      (and (> 0 i) (> 0 j)) 1
      :else 0)
    0))

(defn letter-generator [char]
  (cond
    (= \M char) 2
    (= \S char) 1
    :else 0))

(defn matrix-generator [i j]
  (fn [char] (* (letter-generator char) (power-generator i j))))

(defn apply-matrix [coords arr]
  (let [[x y] coords
        conv-matrix (mapv (fn [i] (mapv (fn [j] ((matrix-generator i j) (get (get arr (+ j y)) (+ i x)))) (range -1 2))) (range -1 2))]
    (reduce + (reduce concat conv-matrix))))

(defn count-christmas [coords arr]
  (let [result (apply-matrix coords arr)]
    (if (= 33 result) 1 0)))

(defn count-words [coords arr]
  (let [[i j] coords
        char (get (get arr j) i)]
    (if (= \A char) (count-christmas coords arr) 0)))

(defn search-word [arr]
  (let [words-found (mapv (fn [i] (mapv (fn [j] (count-words [i j] arr)) (range (count (first arr))))) (range (count arr)))]
    (println words-found)
    (reduce + (reduce concat words-found))))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_4.txt")
        input (slurp input-file)
        char-vec (mapv  #(into [] (seq (char-array %))) (str/split input #"\n"))
        total (search-word char-vec)]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
