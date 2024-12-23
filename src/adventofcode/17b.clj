(ns adventofcode.17b)
(require '[clojure.string :as str])
(require '[clojure.math :as math])
(declare check-possible-bs)
(declare calc-number)
(declare set-A)

(def toint {\0 0 \1 1})

(defn set-A [environment pointer value]
  (assoc environment :A (assoc (environment :A) pointer value)))

(defn try-to-set-A [environment pointer value]
  (if (< pointer 0) (if (= 0 value) [true environment] [false environment])
      (if (= ((environment :A) pointer) value) [true environment]
          (if (= nil ((environment :A) pointer)) [true (set-A environment pointer value)]
              [false environment]))))

(defn make-environment [input]
  (let [lines (str/split-lines input)
        a (bigint (get (str/split (get lines 0) #": ") 1))
        b (Integer/parseInt (get (str/split (get lines 1) #": ") 1))
        c (Integer/parseInt (get (str/split (get lines 2) #": ") 1))
        tape (mapv Integer/parseInt (str/split (get (str/split (get lines 4) #": ") 1) #","))]
    {:A a
     :B b
     :C c
     :tape tape}))

(defn transform-environment [environment]
  (let [environment (dissoc environment :B)
        environment (dissoc environment :C)
        a-length (* 3 (count (environment :tape)))
        new-A (into [] (repeat a-length nil))
        environment (assoc environment :A new-A)]
    environment))

(defn leftpad [string length replchar]
  (if (= length (count string)) string
      (recur (str replchar string) length replchar)))

(defn to-binary [num]
  (let [numstr (Integer/toBinaryString num)
        numstr (leftpad numstr 3 \0)
        numchars (into [] (seq (char-array numstr)))
        intnum (mapv #(toint %) numchars)]
    intnum))

(defn calc-bs [environment pointer]
  (let [all-possible-bs (for [i (range 2) j (range 2) k (range 2)] [i j k])
        filter-first (filter #(if (= nil (get (environment :A) pointer)) true (= (get % 0) (get (environment :A) pointer))) all-possible-bs)
        filter-second
        (filter #(if (= nil (get (environment :A) (+ 1 pointer))) true (= (get % 1) (get (environment :A) (+ 1 pointer)))) filter-first)
        filter-third (filter #(if (= nil (get (environment :A) (+ 2 pointer))) true (= (get % 2) (get (environment :A) (+ 2 pointer)))) filter-second)]
    filter-third))

(defn set-b [environment b pointer]
  (let [new-environment (set-A environment pointer (get b 0))
        new-environment (set-A new-environment (+ pointer 1) (get b 1))
        new-environment (set-A new-environment (+ pointer 2) (get b 2))]
    new-environment))

(defn bit-eq [a b]
  (if (= a b) 1 0))

(defn calc-c [b environment pointer]
  (let [tape (environment :tape)
        current-tape-location (- (- (count tape) (/ pointer 3)) 1)
        current-output (get tape current-tape-location)
        tib (to-binary current-output)
        c [(bit-eq (get b 0) (get tib 0)) (bit-xor (get b 1) (get tib 1)) (bit-eq (get b 2) (get tib 2))]]
    (println "testing calculus")
    (println current-output)
    (println tib)
    (println c)
    (println b)
    (println [(bit-xor (bit-xor (get b 0) 1) (get c 0)) (bit-xor (get b 1) (get c 1)) (bit-xor (bit-xor 1 (get b 2)) (get c 2))])
    c))

(defn calc-shift [b] (calc-number (assoc b 2 (bit-xor (get b 2) 1))))

(defn try-to-set-c [c shift environment pointer]
  (let [c-pointer (- pointer shift)
        [could-set-1 new-environment] (try-to-set-A environment c-pointer (get c 0))
        [could-set-2 new-environment] (try-to-set-A new-environment (+ 1 c-pointer) (get c 1))
        [could-set-3 new-environment] (try-to-set-A new-environment (+ 2 c-pointer) (get c 2))
        successful-set (and (and could-set-1 could-set-2) could-set-3)]
    (if successful-set [true new-environment] [false environment])))

(defn find-A [environment pointer]
  (println environment)
  (if (> 0 pointer) [true environment]
      (let [possible-b (calc-bs environment pointer)]
        (check-possible-bs environment pointer possible-b))))

(def check-possible-bs (fn [environment pointer set-of-bs]
                         (if (= 0 (count set-of-bs)) [false environment]
                             (let [current-b (first set-of-bs)
                                   new-environment (set-b environment current-b pointer)
                                   current-c (calc-c current-b new-environment pointer)
                                   shift (calc-shift current-b)
                                   [c-possible new-environment] (try-to-set-c current-c shift new-environment pointer)]
                               (if (not c-possible) (check-possible-bs environment pointer (subvec (into [] set-of-bs) 1 (count set-of-bs)))
                                   (let [[possible new-environment] (find-A new-environment (- pointer 3))]
                                     (if possible [possible new-environment]
                                         (check-possible-bs environment pointer (subvec (into [] set-of-bs) 1 (count set-of-bs))))))))))

(defn calc-number-helper [A it total]
  (if (= 0 (count A)) total
      (recur (butlast A) (+ 1 it) (+ total (* (last A) (bigint (math/pow 2 it)))))))

(defn calc-number [A]
  (calc-number-helper A 0 (bigint 0)))

(defn -main [& args]
  (let [input-file (or (first args) "fixtures/input_test_17.txt")
        input (slurp input-file)
        environment (make-environment input)
        environment (transform-environment environment)
        [_ environment] (find-A environment (- (* 3 (count (environment :tape))) 3))
        A (environment :A)
        number (calc-number A)]
    (println number)))

(when (= *command-line-args* ["run"])
  (-main))
