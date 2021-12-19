(ns advent-of-code-clj.2021.day-05
  (:require [advent-of-code-clj.core :refer [get-input get-lines]]))


;; --- Day 5: Hydrothermal Venture ---
;; https://adventofcode.com/2021/day/5

(def input (get-lines 2021 5))

(defn parse-line [line]
  (let [[x1 y1 x2 y2]
        (as-> line $
          (re-seq #"\d+" $)
          (map #(Integer/parseInt %) $))]
    {:x1 x1 :y1 y1 :x2 x2 :y2 y2}))

(defn parse-list [input]
  (map parse-line input))

(defn is-vertical? [line]
  (let [{:keys [x1 x2]} line]
    (= x1 x2)))

(defn is-horizontal? [line]
  (let [{:keys [y1 y2]} line]
    (= y1 y2)))

(defn get-hv-lines [input]
  (let [lines (parse-list input)]
    (filter #(or (is-horizontal? %) (is-vertical? %)) lines)))

(defn get-range [start end]
  (let [s (min start end)
        e (max start end)]
    (range s (inc e))))

(defn get-coordinates [line]
  (let [{:keys [x1 y1 x2 y2]} line]
    (cond
      (is-horizontal? line) (map #(vector % y1) (get-range x1 x2))
      (is-vertical? line) (map #(vector x1 %) (get-range y1 y2)))))

(defn part-1 []
  (let [input (get-lines 2021 5)]
    (as-> input $
      (map #(get-coordinates %)
           (parse-list $))
      (apply concat $)
      (frequencies $)
      (filter #(< 1 (val %)) $)
      (count $))))

(part-1)



(comment
  (map #(get-coordinates %)
       (parse-list input))
  (def coords
    (map #(get-coordinates %)
         (parse-list input)))
  coords
  (as-> input $
    (map #(get-coordinates %)
         (parse-list $))

    (apply concat $)
    (frequencies $)
    (filter #(< 1 (val %)) $)
    (count $))

  (frequencies (apply concat coords))
  (vector 2 3)
  (map #(vector % 1) (get-range 2 3))
  (get-coordinates  {:x1 663, :y1 895, :x2 663, :y2 559})
  (get-range 895 559)
  (range 2 9)
  (defn my-range
    [x y]
    (if (<= x y)
      (range x (inc y))
      (reverse (range y (inc x)))))

  (my-range 1 20)
  (for [x 2 y 3] [x y])
  input
  (parse-line "825,870 -> 60,105")
  (parse-list input)
  ;; horizontal or vertical line
  (get-hv-lines input)
  '())
