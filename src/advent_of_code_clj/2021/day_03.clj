(ns advent-of-code-clj.2021.day-03
  (:require [advent-of-code-clj.core :refer [get-input get-lines]]
            [clojure.string :refer [split-lines join]]
            [clojure.repl :refer [doc]]))


;; --- Day 3: Binary Diagnostic ---
;; https://adventofcode.com/2021/day/3


(def input
  (split-lines (get-input 2021 03)))
;=> ["000001000101" "101011100101" ...]

(defn transpose [m]
  (apply mapv vector m))
;=> [[\1  \1  \1  \0  \1  \0  \1  \1  \1  \0  \1  \0 ...] [...]]

(defn parse-binary [s]
  (Integer/parseInt s 2))
; "000010111101" => 189

(defn get-value [input op]
  (as-> input $
    (transpose $)
    (map #(frequencies %) $) ; ({\0 511, \1 489} {\0 505, \1 495})
    (map #(if (op (get % \0) (get % \1)) 1 0) $)
    (join $)
    (parse-binary $)))

(defn get-gamma [input]
  (get-value input <))

(defn get-epsilon [input]
  (get-value input >))

(defn part-1 []
  (let [gamma (get-gamma input)
        epsilon (get-epsilon input)]
    (* gamma epsilon)))

(part-1)


(defn get-zeros [xs pos]
  (filter #(= \0 (nth % pos)) xs))

(defn get-ones [xs pos]
  (filter #(= \1 (nth % pos)) xs))

(defn get-list "list filtering" [xs pos type]
  (let [zeros (get-zeros xs pos)
        count_zeros (count zeros)
        ones (get-ones xs pos)
        count_ones (count ones)]
    (case type
      :most (if (>= count_ones count_zeros)
              ones
              zeros)
      :least (if (>= count_ones count_zeros)
               zeros
               ones))))

(defn get-rating [xs type]
  (loop [xs xs
         pos 0]
    (if (= (count xs) 1)
      (first xs)
      (recur (get-list xs pos type) (inc pos)))))

(defn part-2 []
  (let [data (get-lines 2021 03)
        o2 (get-rating data :most)
        o2-int (parse-binary o2)
        c2 (get-rating data :least)
        c2-int (parse-binary c2)]
    (* o2-int c2-int)))

(part-2)



(comment
  ;; oxygen generator rating
  (def zeros
    (filter #(= \0 (nth % 0)) input))
  (count zeros)
  ;; :most :least
  (get-rating input :most)
  (parse-binary (get-rating input :most))

  (first input)
  (doc filter)
  (nth "001100011100" 2)
  (seq "001100011100")
  (defn get-rating [xs op]
    (loop [xs (split-lines xs)
           pos 0]
      (if (= (count xs) 1)
        (parse-binary (first xs))
        (let [zeros (->> xs (filter #(= \0 (nth % pos))))
              ones  (->> xs (filter #(= \1 (nth % pos))))]
          (if (op (count zeros) (count ones))
            (recur ones  (inc pos))
            (recur zeros (inc pos)))))))

  (get-gamma input)
  (get-epsilon input)

  (as-> input $
    (transpose $)
    (map #(frequencies %) $) ; ({\0 511, \1 489} {\0 505, \1 495})
    (map #(if (< (get % \0) (get % \1)) 1 0) $)
    (join $)
    (parse-binary $)
    #_(map #(sort %) $))

  input

  (transpose [[1 2] [3 4]])

  (doc mapv)

  (defn input->seq
    [input]
    (map seq input))

  (input->seq input)

  (as-> input $
    (str/split-lines $)
    (frequencies $))
  '())
