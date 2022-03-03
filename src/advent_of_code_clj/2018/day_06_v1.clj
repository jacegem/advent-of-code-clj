(ns advent-of-code-clj.2018.day-06-v1
  (:require [clojure.string :as string]))

(defn read-file [year day & {:keys [type] :as opts}]
  (let [filepath (if (= type :sample)
                   (format "resources/%d/input_%02d_sample.txt" year day)
                   (format "resources/%d/input_%02d.txt" year day))]
    (-> (slurp filepath)
        string/split-lines)))

(defn coordinate
  "좌표 정보로 변경"
  {:test #(do (assert (= (coordinate "44, 132")
                         {:x 44 :y 132})))}
  [s]
  (let [[x y] (->> (re-seq #"\d+" s)
                   (map #(Integer/parseInt %)))]
    {:x x :y y}))

(defn manhattan-distance
  "manhatann 거리"
  {:test #(do (assert (= (manhattan-distance {:x 3 :y 4} {:x 1 :y 2})
                         4)))}
  [{cx :x cy :y :as coord} {px :x py :y :as point}]
  (+ (Math/abs (- px cx))
     (Math/abs (- py cy))))

(defn points->boundary
  "경계값 반환"
  {:test #(do (assert (= (points->boundary '({:x 1 :y 2}))
                         {:x-min 1, :x-max 1, :y-min 2, :y-max 2})))}
  [coords]
  (let [xs (map :x coords)
        ys (map :y coords)
        x-min (apply min xs)
        x-max (apply max xs)
        y-min (apply min ys)
        y-max (apply max ys)]
    {:x-min x-min, :x-max x-max, :y-min y-min, :y-max y-max}))


(defn closest-point
  "가장 가까운 포인트 찾기"
  {:test
   #(do (assert (= (closest-point {:x 1 :y 2} '({:x 1 :y 1} {:x 2 :y 3}))
                   {:coord {:x 1, :y 2}, :point {:x 1, :y 1}, :distance 1})))}
  [coord points]
  (let [coord-points-distance (map (fn [point]
                                     {:coord coord :point point :distance (manhattan-distance coord point)})
                                   points)
        min-distance (->> (map :distance coord-points-distance)
                          (apply min))
        min-distance-points (filter (fn [coord-point-distance]
                                      (= min-distance (:distance coord-point-distance)))
                                    coord-points-distance)]
    (when (= (count min-distance-points) 1)
      (first min-distance-points))))
;; (test #'closest-point)

(defn boundary?
  "경계 여부 확인"
  {:test
   #(do (assert (= (boundary? {:x 1 :y 2} {:x-min 0 :x-max 2 :y-min 3 :y-max 4}) false))
        (assert (= (boundary? {:x 2 :y 2} {:x-min 0 :x-max 2 :y-min 3 :y-max 4}) true)))}
  [{:keys [x y]} {:keys [x-min x-max y-min y-max]}]
  (or (= x x-min)
      (= x x-max)
      (= y y-min)
      (= y y-max)))
;; (test #'boundary?)

(defn coord->point
  "각 좌표에서 가장 가까운 포인트와 거리
   같은 거리에 2개 이상의 포인트가 있는 경우 -> nil"
  {:test
   #(do (assert (= (coord->point {:x-min 0 :x-max 2 :y-min 0 :y-max 2} '({:x 1 :y 1} {:x 2 :y 2}))
                   '({:coord {:x 0, :y 0}, :point {:x 1, :y 1}, :distance 2, :boundary true}
                     {:coord {:x 0, :y 1}, :point {:x 1, :y 1}, :distance 1, :boundary true}
                     nil
                     {:coord {:x 1, :y 0}, :point {:x 1, :y 1}, :distance 1, :boundary true}
                     {:coord {:x 1, :y 1}, :point {:x 1, :y 1}, :distance 0, :boundary false}
                     nil
                     nil
                     nil
                     {:coord {:x 2, :y 2}, :point {:x 2, :y 2}, :distance 0, :boundary true}))))}
  [{:keys [x-min x-max y-min y-max] :as boundary} points]
  (for [x (range x-min (inc x-max))
        y (range y-min (inc y-max))]
    (let [coord {:x x :y y}
          coord-point (closest-point coord points)]
      (when coord-point
        (assoc coord-point :boundary (boundary? coord boundary))))))
;; (test #'coord->point)

(defn part-1 []
  (let [points (->> (read-file 2018 6)
                    (map coordinate))
        boundary (points->boundary points)
        all-coord-point (coord->point boundary points)
        boundary-points (->> (filter #(:boundary %) all-coord-point)
                             (group-by :point)
                             keys)]
    (->> all-coord-point
         (remove (fn [coord-point]
                   ((set boundary-points) (:point coord-point))))
         (map :point)
         frequencies
         (sort-by val >)
         first)))

(defn coord-distance-sum
  "각 좌표에서 각 포인트까지의 거리의 합"
  {:test
   #(do (assert (= (coord-distance-sum {:x-min 0 :x-max 4 :y-min 0 :y-max 4} '({:x 1 :y 2} {:x 3 :y 4}))
                   '(10 8 6 6 6 8 6 4 4 4 8 6 4 4 4 8 6 4 4 4 10 8 6 6 6))))}
  [{:keys [x-min x-max y-min y-max] :as boundary} points]
  (for [x (range x-min (inc x-max))
        y (range y-min (inc y-max))]
    (let [coord {:x x :y y}]
      (->> (map #(manhattan-distance coord %) points)
           (apply +)))))

(coord-distance-sum {:x-min 0 :x-max 4 :y-min 0 :y-max 4} '({:x 1 :y 2} {:x 3 :y 4}))

(defn part-2 []
  (let [points (->> (read-file 2018 6)
                    (map coordinate))
        boundary (points->boundary points)]
    (->> (coord-distance-sum boundary points)
         (filter #(> 10000 %))
         count)))

(comment
  (part-1)
  (part-2)

  (read-file 2018 6 :type :sample)
  '())

;; (coord-distance-sum {:x-min 0 :x-max 4 :y-min 0 :y-max 4} '({:x 1 :y 2} {:x 3 :y 4}))
;; ((set '({:x 1, :y 1} {:x 1, :y 6} {:x 8, :y 9} {:x 8, :y 3}))
;;  {:x 1 :y 1})
;; (let [points (->> (read-file 2018 6)
;;                   (map coordinate))
;;       {:keys [x-min x-max y-min y-max] :as bound} (boundary points)
;;       coord-points (apply concat (for [x (range x-min (inc x-max))
;;                                        y (range y-min (inc y-max))]
;;                                    (->> (coord-distances x y points)
;;                                         (closest-point)
;;                                         (check-boundary x y bound))))
;;       infinite-points (->> (filter #(= (:boundary %) false) coord-points)
;;                            (group-by :point)
;;                            keys)
;;       min-distance-point (->> (group-by :coord coord-points)
;;                               vals
;;                               (map #(min-distance-point %)))]