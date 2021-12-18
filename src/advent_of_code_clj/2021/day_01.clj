(ns advent-of-code-clj.2021.day-01
  (:require [advent-of-code-clj.core :as core]
            [clojure.repl :refer [doc]]))

;; -- dya1: Sonar Sweep --
;; https://adventofcode.com/2021/day/1

(def input (core/get-input 2021 01))

(defn read-numbers [data]
  (->> data
       (re-seq #"-?\d+")
       (map #(Integer/parseInt %))))

(defn solve [input]
  (as-> input $
    (read-numbers $)
    (partition 2 1 $)
    (map #(< (first %) (second %)) $)
    (filter true? $)
    (count $)))

(solve input)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comment

(comment
  (defn part-2
    "Day 01 Part 2"
    [input]
    (as-> input $
      (read-numbers $)
      (comb/combinations $ 3)
      (filter #(= 2020 (apply + %)) $)
      (apply * (first $))))

  (defn part-2
    "Day 01 Part 2"
    [input]
    (as-> input $
      (read-numbers $)
      (comb/combinations $ 3)
      (filter #(= 2020 (apply + %)) $)
      (apply * (first $))))

  (defn solve
    "Day 01 Part 1"
    [input]
    (as-> input $
      (read-numbers $)
      (comb/combinations $ 2)
      (filter #(= 2020 (apply + %)) $)
      (apply * (first $))))


  (defn solve [input]
    (->> (partition 2 1 input)
         (map (fn [[a b]] (< a b)))
         (filter true?)
         (count)))

  (partition 2 1 '("123" "456" "789"))
  (partition 2 '("123" "456" "789")))
