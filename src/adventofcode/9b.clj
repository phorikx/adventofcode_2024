(ns adventofcode.9b)
(require '[clojure.string :as str])

(defn sum-helper [start n total]
  (if (= 0 n) total (recur (+ start 1) (- n 1) (+ total start))))

(defn make-starting-file-system [input]
  (let [into_chars (seq (char-array (str/trim input)))
        into_chars (if (= 1 (mod (count into_chars) 2)) into_chars (butlast into_chars))
        file-system (mapv #(Character/digit % 10) into_chars)]
    file-system))

(defn cust-sum [start n]
  (sum-helper start n 0))

(defn free-space-helper [file-system free-spaces counter]
  (if (or (= nil file-system) (>= 1 (count file-system))) free-spaces
      (let [new-free-spaces (conj free-spaces [(+ counter (get file-system 0)) (get file-system 1)])
            new-counter (if (> (count file-system) 1) (+ (+ (get file-system 1) (get file-system 0)) counter) 0)
            new-file-system (subvec file-system 2 (count file-system))]
        (recur new-file-system new-free-spaces new-counter))))

(defn make-free-spaces [file-system]
  (free-space-helper file-system [] 0))

(defn file-helper [file-system files counter-location counter-file]
  (if (or (= nil file-system) (= 0 (count file-system))) files
      (let [new-files (conj files [counter-location (get file-system 0) counter-file])
            new-counter-location (if (> (count file-system) 1) (+ (+ (get file-system 1) (get file-system 0)) counter-location) 0)
            new-file-system (if (> (count file-system) 1) (subvec file-system 2 (count file-system)) nil)]
        (recur new-file-system new-files new-counter-location (+ counter-file 1)))))

(defn matching-space-helper [free-spaces file index]
  (if (>= index (count free-spaces)) nil
      (let [current-space (get free-spaces index)
            current-space-loc (get current-space 0)
            current-space-length (get current-space 1)
            file-length (get file 1)]
        (if (>= current-space-loc (get file 0)) nil
            (if (>= current-space-length file-length) index (recur free-spaces file (+ 1 index)))))))

(defn find-matching-space [free-spaces file]
  (matching-space-helper free-spaces file 0))

(defn calc-sub-total [matching-space file]
  (let [file-number (get file 2)]
    (if (= nil matching-space) (* file-number (cust-sum (get file 0) (get file 1)))
        (* file-number (cust-sum (get matching-space 0) (get file 1))))))

(defn get-diss-loc-helper [free-spaces file-location index]
  (if (>= index (count free-spaces)) (count free-spaces)
      (if (> (get (get free-spaces index) 0) file-location) index
          (recur free-spaces file-location (+ index 1)))))

(defn get-diss-loc [free-spaces file-location] (get-diss-loc-helper free-spaces file-location 0))

(defn add-new-space [free-spaces diss-location file]
  (let [updated-free-spaces (into [] (concat (subvec free-spaces 0 diss-location) [[(get file 0) (get file 1)]]
                                             (subvec free-spaces diss-location (count free-spaces))))]
    updated-free-spaces))

(defn merge-previous [free-spaces diss-location]
  (if (= 0 diss-location) [free-spaces diss-location]
      (let [previous-space (get free-spaces (- diss-location 1))
            current-space (get free-spaces diss-location)]
        (if (= (get current-space 0) (+ (get previous-space 0) (get previous-space 1)))
          (let [updated-free-spaces (into [] (concat (subvec free-spaces 0 (- diss-location 1))
                                                     [[(get previous-space 0) (+ (get previous-space 1) (get current-space 1))]]
                                                     (subvec free-spaces (+ diss-location 1) (count free-spaces))))
                updated-location (- diss-location 1)]
            [updated-free-spaces updated-location])
          [free-spaces diss-location]))))

(defn merge-after [free-spaces diss-location]
  (let [current-space (get free-spaces diss-location)
        next-space (get free-spaces (+ 1 diss-location))
        updated-spaces (if (= (get next-space 0) (+ (get current-space 0) (get current-space 1)))
                         (into [] (concat (subvec free-spaces 0 diss-location)
                                          [[(get current-space 0) (+ (get current-space 1) (get next-space 1))]]
                                          (subvec free-spaces (+ diss-location 2) (count free-spaces))))
                         free-spaces)]
    updated-spaces))

(defn update-free-spaces [free-spaces matching-space-location file]
  (if (= nil matching-space-location) free-spaces
      (let [file-length (get file 1)
            matching-space (get free-spaces matching-space-location)
            matching-space-length (get matching-space 1)
            updated-free-spaces (if (= matching-space-length file-length)
                                  (into [] (concat (subvec free-spaces 0 matching-space-location)
                                                   (subvec free-spaces (+ 1 matching-space-location) (count free-spaces))))
                                  (into [] (concat (subvec free-spaces 0 matching-space-location)
                                                   [[(+ file-length (get matching-space 0)) (- matching-space-length file-length)]]
                                                   (subvec free-spaces (+ 1 matching-space-location) (count free-spaces)))))
            diss-location (get-diss-loc updated-free-spaces (get file 0))
            updated-free-spaces (add-new-space updated-free-spaces diss-location file)
            [updated-free-spaces diss-location] (merge-previous updated-free-spaces diss-location)
            updated-free-spaces (merge-after updated-free-spaces diss-location)]
        updated-free-spaces)))

(defn count-files [files free-spaces total]
  (if (= 0 (count files)) total
      (let [file (last files)
            new-files (butlast files)
            matching-space-location (find-matching-space free-spaces file)
            matching-space (get free-spaces matching-space-location)
            new-free-spaces (update-free-spaces free-spaces matching-space-location file)
            new-total (+ total (calc-sub-total matching-space file))]
        (recur new-files new-free-spaces new-total))))

(defn make-files [file-system]
  (file-helper file-system [] 0 0))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_9.txt")
        input (slurp input-file)
        file-system (make-starting-file-system input)
        files (make-files file-system)
        free-spaces (make-free-spaces file-system)
        total (count-files files free-spaces 0)]
    (println total)))

(when (= *command-line-args* ["run"])
  (-main))
