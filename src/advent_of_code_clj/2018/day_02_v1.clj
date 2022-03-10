(ns advent-of-code-clj.2018.day-02-v1
  (:require [clojure.java.io :as io]
            [clojure.string :refer [trim-newline split-lines]]))

;; https://adventofcode.com/2018/day/2
;; --- Day 2: Inventory Management System ---

(defn read-file [year day]
  (-> (format "%d/input_%02d.txt" year day)
      (io/resource)
      (slurp)
      (trim-newline)
      (split-lines)))

(defn times-count [input n]
  (let [result (map (fn [line] (-> line
                                   frequencies
                                   vals
                                   set)) input)]
    (->> (filter #(% n) result)
         count)))

(defn part-1 []
  (let [input (read-file 2018 2)
        two-count (times-count input 2)
        three-count (times-count input 3)]
    (* two-count three-count)))


(comment
  (part-1)

  '())
