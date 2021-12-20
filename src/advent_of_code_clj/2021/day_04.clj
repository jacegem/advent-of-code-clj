(ns advent-of-code-clj.2021.day-04
  (:require [advent-of-code-clj.core :refer [get-input get-lines]]
            [clojure.string :refer [split-lines join]]
            [clojure.repl :refer [doc]]))


;; --- Day 4: Giant Squid ---
;; https://adventofcode.com/2021/day/4

(defn bingo? [])

(defn part-1 [])


(comment
  (def input (get-lines 2021 04))
  input
  (let [[numbers & boards] input]
    {:numbers numbers
     :boards boards})

  (def boards-data (rest input))
  (def boards (partition 6 boards-data))
  (def board (first boards))
  board
  (re-seq #"\d+" (next board))

  (defn parse-numbers [s]
    (re-seq #"\d+" s))

  (defn parse-board [xs]
    (parse-numbers (next xs)))

  (parse-board boards)

  (next board)
  (next boards)

  (defn parse-numbers [s]
    (re-seq #"\d+" s))

  (defn parse-board [xs]
    (mapv parse-numbers (next xs)))
  
  (def board (first boards))
  board
  (parse-board board))


  (defn parse-input [input]
    (let [[numbers & boards] input]
      {:numbers (parse-numbers numbers)
       :boards  (set (map parse-board (partition 6 boards)))}))
  
  (parse-input input)
  ;; (defn parse-board [xs]
  ;;   (mapv parse-numbers (next xs)))

;; https://github.com/zelark/AoC-2021/blob/main/src/zelark/aoc_2021/day_04.clj  
;; https://github.com/madbence/aoc-21-clj/blob/main/src/madbence/aoc_21/day_04.clj
  '())