(ns advent-of-code-clj.2018.day-06
  (:require [clojure.string :as string]))

;; --- Day 6: Chronal Coordinates ---
;; https://adventofcode.com/2018/day/6

;; 1, 1 -> A
;; 1, 6 -> B
;; 8, 3 -> C
;; 3, 4 -> D
;; 5, 5 -> E
;; 8, 9 -> F
;; {:x 1 :y 2}


(defn read-file [year day]
  (-> (format "resources/%d/input_%02d.txt" year day)
      slurp
      string/split-lines))

(defn coordinate
  {:test #(do (assert (= (coordinate "44, 132") {:x 44 :y 132})))}
  [s]
  (let [[x y] (->> (re-seq #"\d+" s)
                   (map #(Integer/parseInt %)))]
    {:x x :y y}))
;; (test #'coordinate)

(defn manhattan-distance
  {:test #(do (assert (= (manhattan-distance 3 4 {:x 1 :y 2}) 4)))}
  [x y {px :x py :y :as point}]
  (+ (Math/abs (- px x))
     (Math/abs (- py y))))
;; (test #'manhattan-distance)

(defn coord-distances
  {:test
   #(do (assert (= (coord-distances 1 1 '({:x 2 :y 2} {:x 3 :y 3}))
                   '({:coord {:x 1, :y 1}, :point {:x 2, :y 2}, :distance 2}
                     {:coord {:x 1, :y 1}, :point {:x 3, :y 3}, :distance 4}))))}
  [x y points]
  (map
   (fn [point]
     {:coord {:x x :y y} :point point :distance (manhattan-distance x y point)})
   points))
;; (test #'coord-distances)

;; (coord-distances 1 1 '({:x 2 :y 2} {:x 3 :y 3}))
;; (distances 1 2 '({:x 2 :y 3} {:x 4 :y 5}))
;; (min-distance 1 2 '({:x 2 :y 3} {:x 4 :y 5}))

(defn min-distance-point
  {:test
   #(do (assert (= (min-distance-point '({:coord {:x 1, :y 1}, :point {:x 2, :y 2}, :distance 2}
                                         {:coord {:x 1, :y 1}, :point {:x 3, :y 3}, :distance 4}))
                   {:x 2, :y 2})))}
  [coord-point-distances]
  (let [min-distance (->> (map :distance coord-point-distances)
                          (apply min))
        coord-points (filter
                      (fn [coord-distance]
                        (= min-distance (:distance coord-distance))) coord-point-distances)
        min-distance-count (count coord-points)]
    (when (= min-distance-count 1)
      (:point (first coord-points)))))
(test #'min-distance-point)

(let [coords (->> (read-file 2018 6)
                  (map coordinate))
      xs (map :x coords)
      ys (map :y coords)
      x-min (apply min xs)
      x-max (apply max xs)
      y-min (apply min ys)
      y-max (apply max ys)
      points (for [x (range x-min (inc x-max))
                   y (range y-min (inc y-max))]
               (->> (coord-distances x y coords)
                    min-distance-point))]
  (->> points
       frequencies
       (sort-by val)
       last))



(->>
 (frequencies '({:x 42, :y 76} {:x 42, :y 76} {:x 293, :y 356}))
 (sort-by val)
 last)

(def ds
  '({:coord {:x 2, :y 3}, :distance 2} {:coord {:x 4, :y 5}, :distance 6}))

(frequencies ds)

(->>
 (frequencies (map :distance ds))
 (vals max-key))
(apply min-key :distance ds)





