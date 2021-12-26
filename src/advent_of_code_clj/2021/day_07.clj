(ns advent-of-code-clj.2021.day-07
  (:require [advent-of-code-clj.core :refer [get-input get-lines]]))

;; --- Day 7: The Treachery of Whales ---
;; https://adventofcode.com/2021/day/7

(defn diff [v1 v2]
  (Math/abs (- v1 v2)))

(defn parse [numbers]
  (map #(Integer/parseInt %) (re-seq #"\d+" numbers)))

(defn gauss-sum
  [n]
  (/ (* n (inc n)) 2))

(defn part-1 []
  (let [input (get-input 2021 7)
        numbers (parse input)
        min-pos (apply min numbers)
        max-pos (apply max numbers)]
    (apply min
           (for [num (range min-pos (inc max-pos))]
             (apply +
                    (for [pos numbers]
                      (diff pos num)))))))

(part-1)

(defn part-2 []
  (let [input (get-input 2021 7)
        numbers (parse input)
        min-pos (apply min numbers)
        max-pos (apply max numbers)]
    (apply min
           (for [num (range min-pos (inc max-pos))]
             (apply +
                    (for [pos numbers]
                      (gauss-sum (diff pos num))))))))

(part-2)



(comment
  (def input "16,1,2,0,4,2,7,1,2,14")



  (def input (get-input 2021 7))
  input

  (def numbers (parse input))
  numbers
  ; min ~ max
  (def min-pos (apply min numbers))
  min-pos
  (def max-pos (apply max numbers))
  max-pos



  (apply min
         (for [num (range min-pos (inc max-pos))]
           (apply +
                  (for [pos numbers]
                    (gauss-sum (diff pos num))))))

  ;;   (as-> $
  ;;     (parse $)



  (def avg
    (as-> numbers $
      (reduce + $)
      (/ $ (count numbers))
      (int $)))
  (def sum
    (as-> numbers $
      (reduce + $)))
  sum
  avg


  (def fuel
    (as-> avg $
      (map #(diff $ %) numbers)
      (reduce + $)))
  fuel


  '())
