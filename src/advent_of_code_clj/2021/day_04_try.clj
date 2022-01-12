(ns advent-of-code-clj.2021.day-04-try
  (:require [advent-of-code-clj.core :refer [get-input get-lines]]
            [clojure.string :refer [split-lines join]]
            [clojure.repl :refer [doc]]))


;; --- Day 4: Giant Squid ---
;; https://adventofcode.com/2021/day/4

(defn parse-numbers [line]
  (re-seq #"\d+" line))

(defn transpose [m]
  (apply map vector m))

(defn make-board [lines]
  (concat (map set lines)
          (map set (transpose lines))))

(defn parse-boards [lines]
  (as-> (map parse-numbers lines) $
    (partition-by empty? $)
    (filter #(seq (first %)) $)
    (make-board $)))


(defn parse [input]
  (let [[numbers & boards] input]
    {:numbers (parse-numbers numbers)
     :boards (parse-boards boards)}))

(defn mark-boards [boards n]
  (map (fn [board]
         (map #(disj % n) board))
       boards))

(defn get-bingo [boards]
  (first (filter #(some empty? %) boards)))

(defn get-score [data]
  (loop [numbers (:numbers data)
         boards (:boards data)]
    (let [n (first numbers)
          boards (mark-boards boards n)
          bingo (get-bingo boards)]
      (if bingo
        (/ (* n ((reduce + (mapcat reverse bingo)))) 2)
        (recur (rest numbers) (rest boards))))))

(defn part-1 []
  (let [input (get-lines 2021 04)]
    (as-> (parse input) $
      (get-score $))))

(part-1)

(comment
  (def input (get-lines 2021 04))
  input

  (mark-boards (list (list "58" "14" "78" "65" "38")
                     (list "36" "11" "70" "77" "80"))
               "11")

  (defn parse-numbers [line]
    (re-seq #"\d+" line))

  (defn transpose [m]
    (apply map vector m))

  (defn make-board [lines]
    (concat (map set lines)
            (map set (transpose lines))))

  (defn parse-boards [lines]
    (as-> (map parse-numbers lines) $
      (partition-by empty? $)
      (filter #(seq (first %)) $)))

  (defn parse [input]
    (let [[numbers & boards] input]
      {:numbers (parse-numbers numbers)
       :boards (parse-boards boards)}))

  (def data (parse input))
  data

  (defn mark-boards [boards n]
    (map (fn [board]
           (map #(disj % n) board))
         boards))

  (defn get-bingo [boards]
    (first (filter #(some empty? %) boards)))

  (defn get-score [board]
    (reduce + board))
    ;; (flatten board)

  (loop [numbers (:numbers data)
         boards (:boards data)]
    (let [n (first numbers)
          boards (mark-boards boards n)
          bingo (get-bingo boards)]
      (if bingo
        ;; (* n (get-score bingo))
        (print bingo)
        (recur (rest numbers) (rest boards)))))

  (parse input)

  (def bingo (list #{28} #{59 67 45} #{51 68 6 92} #{69 8 84} #{26} #{51 8 67} #{} #{69 28 92} #{26 6 84 45} #{59 68}))

    ;; (map #(prn %) board)
    ;; (reduce + board)

  (reduce + (mapcat reverse bingo))

  (get-score bingo)
  bingo
  (re-seq #"\d+" "12")



  (defn parse-line [s]
    (map #(Integer/parseInt %) (re-seq #"\d+" s)))

  (map #(parse-line %) input)

  (parse-line "")






    ;; (->> lines
    ;;      (partition-by empty?)
    ;;      (filter #(seq (first %)))
    ;;      #_(map make-board)))
  (def board (list "11 73 88 47 30"
                   "10  6  5 25 67"
                   "89 41 62 94 85"
                   "45 99 58  7 57"
                   "77 19 66 43 48"))
  (parse-numbers board)
  (board)
  (partition-by empty? board)
  ; (("") ("11 73 88 47 30" "10  6  5 25 67" "89 41 62 94 85" "45 99 58  7 57" "77 19 66 43 48"))
  (defn transpose [a]
    (map
     (fn [i]
       (map #(nth % i) a))
     (range (count (first a)))))

  (rest input)
  (parse-boards (rest input))
  (map parse-numbers (rest input))

  (defn parse [input]
    (let [[numbers & boards] input]
      {:numbers (parse-numbers numbers)
       :boards (parse-boards boards)}))
  (def data (parse input))

  data
  (defn mark-boards [boards n]
    (map (fn [board]
           (map #(disj % n) board))
         boards))

  (mark-boards (:boards data) 2)

  (:boards data)
  (defn get-bingo [boards]
    (first (filter #(some empty? %) boards)))

  (get-bingo (:boards data))

  (defn get-score [board]
    (reduce + board))


  (loop [numbers (:numbers data)
         boards (:boards data)]
    (let [n (first numbers)
          boards (mark-boards boards n)
          bingo (get-bingo boards)]
      (if bingo
        ;; (* n (get-score bingo))
        (print bingo)
        (recur (rest numbers) (rest boards)))))


  (def data
    (let [[numbers & boards] input]
      {:numbers numbers
       :boards boards}))
  data
  (def boards-data (rest input))
  (def boards (partition 6 (:boards data)))
  (def board (first boards))
  board

  (re-seq #"\d+" (:boards data))

  (:boards data)


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
  (parse-board board)


  (defn transpose [m]
    (apply map vector m))

  (defn make-board [lines]
    (concat (map set lines)
            (map set (transpose lines))))

  (def board '(("36" "11" "70" "77" "80")
               ("63" "3" "56" "75" "28")
               ("89" "91" "27" "33" "82")
               ("53" "79" "52" "96" "32")
               ("58" "14" "78" "65" "38")))
  (make-board board)

  (transpose board)
  (vector map board)

  (defn parse-input [input]
    (let [[numbers & boards] input]
      {:numbers (parse-numbers numbers)
       :boards  (set (map parse-board (partition 6 boards)))}))

  (parse-input input)
  ;; (defn parse-board [xs]
  ;;   (mapv parse-numbers (next xs)))

;; https://github.com/madbence/aoc-21-clj/blob/main/src/madbence/aoc_21/day_04.clj
;; https://github.com/zelark/AoC-2021/blob/main/src/zelark/aoc_2021/day_04.clj  
;; https://github.com/madbence/aoc-21-clj/blob/main/src/madbence/aoc_21/day_04.clj
  '())