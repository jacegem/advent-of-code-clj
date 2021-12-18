(ns advent-of-code-clj.2021.day-02
  (:require [advent-of-code-clj.core :as core]
            [clojure.repl :refer [doc]]))


;; --- Day 2: Dive! ---
;; https://adventofcode.com/2021/day/2
;; 
;; 3가지 경우
;; forward, up, down

(def input (core/get-input 2021 02))

(defn read-data [input]
  (as-> input $
    (re-seq #"(\w+) (\d+)" $)
    (map (fn [[_ cmd x]]
           [(keyword cmd) (Integer/parseInt x)]) $)))


(defn step-1 [[hpos depth] [cmd x]]
  (case cmd
    :forward [(+ hpos x) depth]
    :up      [hpos (- depth x)]
    :down    [hpos (+ depth x)]))


(defn part-1 [input]
  (as-> input $
    (read-data $)
    (reduce step-1 [0 0] $)
    (apply * $)))

(part-1 input)


(defn step-2 [[hpos depth aim] [cmd x]]
  (case cmd
    :forward [(+ hpos x) (+ depth (* aim x)) aim]
    :up      [hpos depth (- aim x)]
    :down    [hpos depth (+ aim x)]))

(defn part-2 [input]
  (as-> input $
    (read-data $)
    (reduce step-2 [0 0 0] $)
    (take 2 $)
    (apply * $)))

(part-2 input)

(comment
  (read-data input)
  (->> (read-data input)
       (reduce step-1 [0 0])
       (apply *)) ; 1727835
  (as-> input $
    (read-data $)
    (reduce step-2 [0 0 0] $)
    (take 2 $)
    (apply * $)))