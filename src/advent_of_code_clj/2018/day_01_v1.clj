(ns advent-of-code-clj.2018.day-01-v1
  (:require [clojure.java.io :as io]
            [clojure.string :refer [trim-newline split-lines]]))

;; https://adventofcode.com/2018/day/1
;; --- Day 1: Chronal Calibration ---

(defn read-file [year day]
  (-> (format "%d/input_%02d.txt" year day)
      (io/resource)
      (slurp)
      (trim-newline)
      (split-lines)))

(defn part-1 []
  (let [input (read-file 2018 1)
        numbers (map #(Integer/parseInt %) input)]
    (reduce + numbers)))

(defn calibrate [{:keys [reaches result] :as state} change]
  (let [result (+ result change)]
    (if (reaches result)
      (reduced result)
      (-> state
          (assoc :result result)
          (assoc :reaches (conj reaches result))))))

(defn part-2 []
  (let [input (read-file 2018 1)
        numbers (map #(Integer/parseInt %) input)]
    (reduce calibrate {:reaches #{} :result 0} (cycle numbers))))

(comment
  (part-1)
  (part-2)
  '())


