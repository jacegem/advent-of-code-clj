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

(defn vertical? [line]
  (let [{:keys [x1 x2]} line]
    (= x1 x2)))

(defn horizontal? [line]
  (let [{:keys [y1 y2]} line]
    (= y1 y2)))

(defn diff [^int v1 ^int v2]
  (Math/abs (- v2 v1)))

(defn diagonal? [line]
  (let [{:keys [x1 y1 x2 y2]} line]
    (= (diff x1 x2) (diff y1 y2))))

(defn get-range [v1 v2]
  (if (< v1 v2)
    (range v1 (inc v2))
    (reverse (range v2 (inc v1)))))

(defn get-coordinates [line & {:keys [with-diagonal] :or {with-diagonal false}}]
  (let [{:keys [x1 y1 x2 y2]} line]
    (cond
      (horizontal? line) (map #(vector % y1) (get-range x1 x2))
      (vertical? line) (map #(vector x1 %) (get-range y1 y2))
      (and (true? with-diagonal) (diagonal? line)) (map vector (get-range x1 x2) (get-range y1 y2)))))

(defn part-1 []
  (let [input (get-lines 2021 5)]
    (as-> input $
          (map #(get-coordinates % :with-diagonal false)
               (parse-list $))
          (apply concat $)
          (frequencies $)
          (filter #(< 1 (val %)) $)
          (count $))))

(part-1)
;; 6856

(defn part-2 []
  (let [input (get-lines 2021 5)]
    (as-> input $
          (map #(get-coordinates % :with-diagonal true)
               (parse-list $))
          (apply concat $)
          (frequencies $)
          (filter #(< 1 (val %)) $)
          (count $))))

(part-2)
;; 20666

(comment
  ;; (defn get-range [start end]
  ;;   (let [s (min start end)
  ;;         e (max start end)]
  ;;     (range s (inc e))))

  (defn get-hv-lines [input]
    (let [lines (parse-list input)]
      (filter #(or (horizontal? %) (vertical? %)) lines)))
  (get-range 22 4)
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
  (get-coordinates {:x1 663, :y1 895, :x2 663, :y2 559})
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
  ;; https://github.com/yyna/advent-of-code/blob/main/src/_2021/day_5.clj
  '())
