(ns adventofcode.22b)
(require '[modules.vectormath :as vec])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

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

(defn make-sequence-helper [so-far count-down secret-number]
  (if (= 0 count-down) so-far
      (recur (conj so-far secret-number) (- count-down 1) (calculate-next-number secret-number))))

(defn make-sequence [secret-number]
  (make-sequence-helper [] 2001 secret-number))

(defn add-to-map [current-map diff-sequence current-val]
  (if (not (= 4 (count diff-sequence))) current-map
      (if (not (= nil (current-map diff-sequence))) current-map
          (assoc current-map diff-sequence current-val))))

(defn make-sequence-map-helper [some-seq diff-sequence current-map last-val]
  (if (= 0 (count some-seq)) current-map
      (let [current-val (mod (first some-seq) 10)
            dif (if (not (= nil last-val)) (- current-val last-val) nil)
            diff-sequence (if (not (= nil dif)) (conj diff-sequence dif) diff-sequence)
            diff-sequence (if (> (count diff-sequence) 4) (into [] (rest diff-sequence)) diff-sequence)
            current-map (add-to-map current-map diff-sequence current-val)]
        (recur (rest some-seq) diff-sequence current-map current-val))))

(defn make-sequence-map [sequence]
  (make-sequence-map-helper sequence [] {} nil))

(defn default-get [map key]
  (if (not (= nil (map key))) (map key) 0))

(defn get-keys [sequence-maps]
  (reduce set/union (mapv #(into #{} (keys %)) sequence-maps)))

(defn get-key-value [sequence-maps k]
  (reduce + (mapv #(default-get % k) sequence-maps)))

(defn calc-maxi-key-helper [ks sequence-maps highest highest-key]
  (if (= 0 (count ks)) [highest highest-key]
      (let [current-val (get-key-value sequence-maps (first ks))
            new-highest (max current-val highest)
            new-highest-key (if (> current-val highest) (first ks) highest-key)]
        (recur (rest ks) sequence-maps new-highest new-highest-key))))

(defn calc-maxi-key [ks sequence-maps]
  (calc-maxi-key-helper ks sequence-maps 0 0))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_22.txt")
        input (slurp input-file)
        secret-numbers (mapv Integer/parseInt (str/split-lines input))
        sequences (mapv make-sequence secret-numbers)
        sequence-maps (mapv make-sequence-map sequences)
        all-keys (get-keys sequence-maps)
        [maxi-key maxi-value] (calc-maxi-key all-keys sequence-maps)]
    (println maxi-value)
    (println maxi-key)))

(when (= *command-line-args* ["run"])
  (-main))
