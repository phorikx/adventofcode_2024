(ns adventofcode.9a)
(require '[clojure.string :as str])

(defn count-recursively [file-system
                         current_count_position_counter
                         current_total
                         is_free_space
                         begin_index_counter
                         end_index_counter]
  (if (or (= 0 (count file-system)) (= nil file-system)) current_total
      (let
       [zero-at-start (= 0 (get file-system 0))
        file-system (if zero-at-start (subvec file-system 1 (count file-system)) file-system)
        is_free_space (if zero-at-start (not is_free_space) is_free_space)
        begin_index_counter (if (and zero-at-start is_free_space) (+ 1 begin_index_counter) begin_index_counter)
        zero-at-start (= 0 (get file-system 0))
        file-system (if zero-at-start (subvec file-system 1 (count file-system)) file-system)
        check-for-inconsistencies (if (= 0 (get file-system 0)) (throw (Exception. "should not happen - 0 at start")))
        is_free_space (if zero-at-start (not is_free_space) is_free_space)
        file-system (assoc file-system 0 (- (get file-system 0) 1))
        zero-at-end (= 0 (last file-system))
        end_index_counter (if zero-at-end (- end_index_counter 1) end_index_counter)
        file-system (if zero-at-end (into [] (butlast (butlast file-system))) file-system)
        check-for-inconsistencies (if (= 0 (last file-system)) (throw (Exception. "should not happen - 0 at end")) check-for-inconsistencies)
        index-last (- (count file-system) 1)
        file-system (if (and is_free_space (>= index-last 0)) (assoc file-system index-last (- (get file-system index-last) 1)) file-system)
        current_count (if is_free_space end_index_counter begin_index_counter)
        current_count (if (= 0 (count file-system)) 0 current_count)
        current_total (+ current_total (* current_count_position_counter current_count))
        current_count_position_counter (+ current_count_position_counter 1)]
        check-for-inconsistencies
        (if (> 100 (count file-system)) (println file-system))
        (recur file-system current_count_position_counter current_total is_free_space begin_index_counter end_index_counter))))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_9.txt")
        input (slurp input-file)
        into_chars (seq (char-array (str/trim input)))
        length (count into_chars)
        into_chars (if (= 1 (mod length 2)) into_chars (butlast into_chars))
        end_index_counter (int (/ (- length 1) 2))
        file-system (mapv #(Character/digit % 10) into_chars)
        total (count-recursively file-system 0 (bigint 0) false 0 end_index_counter)]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
