(ns adventofcode.21a)
(require '[modules.vectormath :as vec])
(require '[clojure.string :as str])

(def numpad {\7 [0 0]
             \8 [0 1]
             \9 [0 2]
             \4 [1 0]
             \5 [1 1]
             \6 [1 2]
             \1 [2 0]
             \2 [2 1]
             \3 [2 2]
             \0 [3 1]
             \A [3 2]})

(def keypad {\^ [0 1]
             \A [0 2]
             \< [1 0]
             \v [1 1]
             \> [1 2]})

(defn make-numeric [input-row]
  (let [numeric-part (butlast input-row)
        as-str (reduce str numeric-part)
        to-int (Integer/parseInt as-str)]
    to-int))

(defn add-keys [distance
                already-pressed
                cant-go-right
                cant-go-down
                cant-go-up]
  (let [first-coord (get distance 0)
        second-coord (get distance 1)
        new-already-pressed already-pressed
        new-already-pressed (if cant-go-down new-already-pressed (if (> first-coord 0)
                                                                   (concat new-already-pressed (repeat (abs first-coord) \v)) new-already-pressed))
        new-already-pressed (if cant-go-right new-already-pressed (if (< second-coord 0)
                                                                    (concat new-already-pressed (repeat (abs second-coord) \<)) new-already-pressed))
        new-already-pressed (if cant-go-up new-already-pressed (if (< first-coord 0)
                                                                 (concat new-already-pressed (repeat (abs first-coord) \^)) new-already-pressed))
        new-already-pressed (if (> second-coord 0)
                              (concat new-already-pressed (repeat (abs second-coord) \>)) new-already-pressed)
        new-already-pressed (if (not cant-go-right)
                              new-already-pressed
                              (if (< second-coord 0)
                                (concat new-already-pressed (repeat (abs second-coord) \<)) new-already-pressed))
        new-already-pressed (if (not cant-go-up)
                              new-already-pressed
                              (if (< first-coord 0)
                                (concat new-already-pressed (repeat (abs first-coord) \^)) new-already-pressed))
        new-already-pressed (if (not cant-go-down) new-already-pressed (if (> first-coord 0)
                                                                         (concat new-already-pressed (repeat (abs first-coord) \v)) new-already-pressed))
        new-already-pressed (concat new-already-pressed '(\A))]
    new-already-pressed))

(defn key-to-num-helper [to-go current-location already-pressed]
  (if (= 0 (count to-go)) already-pressed
      (let [go-to (first to-go)
            distance (vec/vec-dif (numpad go-to) (numpad current-location))
            cant-go-right (and (= 3 (get (numpad current-location) 0)) (= 0 (get (numpad go-to) 1)))
            cant-go-down (and (= 0 (get (numpad current-location) 1)) (= 3 (get (numpad go-to) 0)))
            new-already-pressed (add-keys distance already-pressed cant-go-right cant-go-down false)]
        (recur (rest to-go) go-to new-already-pressed))))

(defn num-to-num-helper [to-go current-location already-pressed]
  (if (= 0 (count to-go)) already-pressed
      (let [go-to (first to-go)
            distance (vec/vec-dif (keypad go-to) (keypad current-location))
            cant-go-right (and (= 0 (get (keypad current-location) 0)) (= 0 (get (keypad go-to) 1)))
            cant-go-up (= [1 0] (keypad current-location))
            new-already-pressed (add-keys distance already-pressed cant-go-right false cant-go-up)]
        (recur (rest to-go) go-to new-already-pressed))))

(defn num-to-num [input]
  (num-to-num-helper input \A '()))

(defn key-to-num [input]
  (key-to-num-helper input \A '()))

(defn calc-key-presses [input]
  (let [first-robot-press (key-to-num input)
        second-robot-press (num-to-num first-robot-press)
        human-press (num-to-num second-robot-press)]
    human-press))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_21.txt")
        input (mapv #(into [] (seq (char-array %))) (str/split-lines (slurp input-file)))
        keypresses (mapv calc-key-presses input)
        no_of_keypresses (mapv count keypresses)
        numeric (mapv make-numeric input)
        combined (mapv vector numeric no_of_keypresses)
        multiples (mapv #(* (get % 0) (get % 1)) combined)
        total (reduce + multiples)]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
