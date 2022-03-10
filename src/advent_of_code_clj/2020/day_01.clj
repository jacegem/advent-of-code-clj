(ns advent-of-code-clj.2020.day-01
  (:require [clojure.string :as string]))


;; --- Day 1: Report Repair ---
;; https://adventofcode.com/2020/day/1


(defn read-file [year day & {:keys [type] :as opts}]
  (let [filepath (if (= type :sample)
                   (format "resources/%d/input_%02d_sample.txt" year day)
                   (format "resources/%d/input_%02d.txt" year day))]
    (->> (slurp filepath)
         string/split-lines
         (map #(Integer/parseInt %)))))

(defn part-1 []
  (let [numbers (read-file 2020 1)]
    (->>
     (for [x numbers
           y numbers
           :when (and (not (= x y))

                      (= (+ x y) 2020))]
       (* x y))
     first)))

(defn part-2 []
  (let [numbers (read-file 2020 1)]
    (->>
     (for [x numbers
           y numbers
           z numbers
           :when (and (not (= x y z))

                      (= (+ x y z) 2020))]
       (* x y z))
     first)))


(comment
  (part-1)
  (part-2)
  '())


