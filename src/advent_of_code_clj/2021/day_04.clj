(ns advent-of-code-clj.2021.day-04
  (:require [advent-of-code-clj.core :refer [get-input get-lines]]))

;; --- Day 4: Giant Squid ---
;; https://adventofcode.com/2021/day/4

(defn parse-numbers [line]
  (map #(Integer/parseInt %) (re-seq #"\d+" line)))

(defn transpose [m]
  (apply map vector m))

(defn make-board [board]
  (concat (map set board)
          (map set (transpose board))))

(defn make-boards [boards]
  (map #(make-board %) boards))

(defn parse-boards [boards]
  (as-> (map parse-numbers boards) $
    (partition-by empty? $)
    (filter #(seq (first %)) $)
    (make-boards $)))

(defn mark-boards [boards n]
  (map (fn [board]
         (map #(disj % n) board))
       boards))

(defn parse [input]
  (let [[numbers & boards] input
        numbers (parse-numbers numbers)
        boards (parse-boards boards)]
    {:numbers numbers
     :boards boards}))

(defn get-bingo [boards]
  (first (filter #(some empty? %) boards)))

(defn get-score [board n]
  (as-> board $
    (map #(reduce + %) $)
    (reduce + $)
    (/ $ 2)
    (* $ n)))

(defn process [data]
  (loop [numbers (:numbers data)
         boards (:boards data)]
    (let [n (first numbers)
          boards (mark-boards boards n)
          bingo (get-bingo boards)]
      (if bingo
        (get-score bingo n)
        (recur (rest numbers) boards)))))

(defn part-1 []
  (as-> (get-lines 2021 4) $
    (parse $)
    (process $)))

(part-1)

(defn bingo? [board]
  (some empty? board))

(defn process-2 [data stop-condition]
  (loop [numbers (:numbers data)
         boards (:boards data)]
    (let [n (first numbers)
          boards (mark-boards boards n)]
      (if (stop-condition bingo? boards)
        (as-> boards $
          (filter bingo? $)
          (first $)
          (get-score $ n))
        (recur (rest numbers) (filter #(not (bingo? %)) boards))))))

(defn part-2 []
  (as-> (get-lines 2021 4) $
    (parse $)
    (process-2 $ every?)))

(part-2)


(comment
  (def input (get-lines 2021 04))
  input

  (defn parse-numbers [line]
    (map #(Integer/parseInt %) (re-seq #"\d+" line)))

  (defn transpose [m]
    (apply map vector m))

  (defn make-board [board]
    (concat (map set board)
            (map set (transpose board))))

  (defn make-boards [boards]
    (map #(make-board %) boards))

  (defn parse-boards [boards]
    (as-> (map parse-numbers boards) $
      (partition-by empty? $)
      (filter #(seq (first %)) $)
      (make-boards $)))

  (defn mark-boards [boards n]
    (map (fn [board]
           (map #(disj % n) board))
         boards))

  (defn parse [input]
    (let [[numbers & boards] input
          numbers (parse-numbers numbers)
          boards (parse-boards boards)]
      {:numbers numbers
       :boards boards}))
  (parse input)

  (defn get-bingo [boards]
    (first (filter #(some empty? %) boards)))

  (defn get-score [board n]
    (as-> board $
      (reduce + (map #(reduce + %) $))
      (/ $ 2)
      (* n $)))

  (defn process [data]
    (loop [numbers (:numbers data)
           boards (:boards data)]
      (let [n (first numbers)
            boards (mark-boards boards n)
            bingo (get-bingo boards)]
        (if bingo
          (get-score bingo n)
          (recur (rest numbers) boards)))))

  (defn bingo? [board]
    (some empty? board))

  (defn process-2 [data stop-condition]
    (loop [numbers (:numbers data)
           boards (:boards data)]
      (let [n (first numbers)
            boards (mark-boards boards n)]
        (if (stop-condition bingo? boards)
          (as-> boards $
            (filter bingo? $)
            (first $)
            (get-score $ n))
          (recur (rest numbers) (filter #(not (bingo? %)) boards))))))

  (as-> input $
    (parse $)
    (process $))

  (as-> input $
    (parse $)
    (process-2 $ every?))

  (def h (list #{28} #{59 45 67} #{92 6 51 68} #{69 8 84} #{26} #{51 67 8} #{} #{69 92 28} #{6 45 26 84} #{59 68}))
  h
  (map #(prn %) h)
  (reduce + (map #(reduce + %) h))

  (defn parse [input]
    (let [[numbers & boards] input]
      {:numbers (parse-numbers numbers)
       :boards (parse-boards boards)}))

  (defn parse [input]
    (let [[numbers & boards] input]
      {:numbers numbers
       :boards boards}))

  '())
