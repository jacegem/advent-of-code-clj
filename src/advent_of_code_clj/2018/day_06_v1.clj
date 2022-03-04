(ns advent-of-code-clj.2018.day-06-v1
  (:require [clojure.string :as string]))

(defn read-file [year day & {:keys [type] :as opts}]
  (let [filepath (if (= type :sample)
                   (format "resources/%d/input_%02d_sample.txt" year day)
                   (format "resources/%d/input_%02d.txt" year day))]
    (-> (slurp filepath)
        string/split-lines)))
;; (read-file 2018 6)

(defn coordinate-str
  "좌표 정보로 변경"
  {:test #(do (assert (= (coordinate-str "44, 132")
                         {:x 44 :y 132})))}
  [s]
  (let [[x y] (->> (re-seq #"\d+" s)
                   (map #(Integer/parseInt %)))]
    {:x x :y y}))
;; (test #'coordinate-str)

;; input : 입력
;; coord : 좌표
;; input-coord : 문제에서 주어진 좌표
;; grid-coord : 각각의 좌표
;; closest-coord : 가장 가까운 입력 좌표

(defn calc-manhattan-distance
  "manhatann 거리"
  {:test #(do (assert (= (calc-manhattan-distance {:x 3 :y 4} {:x 1 :y 2})
                         4)))}
  [{gx :x gy :y :as grid-coord} {ix :x iy :y :as input-coord}]
  (+ (Math/abs (- ix gx))
     (Math/abs (- iy gy))))
;; (test #'calc-manhattan-distance)

(defn extract-boundary
  "경계값 반환"
  {:test #(do (assert (= (extract-boundary '({:x 1 :y 2} {:x 3 :y 4}))
                         {:x-min 1, :x-max 3, :y-min 2, :y-max 4})))}
  [coords]
  (let [xs (map :x coords)
        ys (map :y coords)
        x-min (apply min xs)
        x-max (apply max xs)
        y-min (apply min ys)
        y-max (apply max ys)]
    {:x-min x-min, :x-max x-max, :y-min y-min, :y-max y-max}))
;; (test #'extract-boundary)
;; extract-boundary

(defn find-closest-input-point
  "가장 가까운 포인트 찾기"
  {:test
   #(do (assert (= (find-closest-input-point {:x 1 :y 2} '({:x 1 :y 1} {:x 2 :y 3}))
                   {:grid-coord {:x 1, :y 2}, :input-coord {:x 1, :y 1}, :distance 1, :closest-input-coord {:x 1, :y 1}})))}
  [grid-coord input-coord]
  (let [grid-with-distances (map (fn [input]
                                   {:grid-coord grid-coord
                                    :input-coord input
                                    :distance (calc-manhattan-distance grid-coord input)})
                                 input-coord)
        min-distance (->> (map :distance grid-with-distances)
                          (apply min))
        min-distance-input-coords (filter (fn [grid-input-distance]
                                            (= min-distance (:distance grid-input-distance)))
                                          grid-with-distances)]
    (when (= (count min-distance-input-coords) 1)
      (let [grid-with-input (first min-distance-input-coords)]
        (assoc grid-with-input :closest-input-coord (:input-coord grid-with-input))))))
(find-closest-input-point {:x 1 :y 2} '({:x 1 :y 1} {:x 2 :y 3}))
;; find-closest-input-point

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

(defn find-each-closest-input-point
  "각 좌표에서 가장 가까운 포인트와 거리
   같은 거리에 2개 이상의 포인트가 있는 경우 -> nil"
  {:test
   #(do (assert (= (find-each-closest-input-point {:x-min 0 :x-max 2 :y-min 0 :y-max 2} '({:x 1 :y 1} {:x 2 :y 2}))
                   '({:grid-coord {:x 0, :y 0}, :input-coord {:x 1, :y 1}, :distance 2, :closest-input-coord {:x 1, :y 1}, :boundary true}
                     {:grid-coord {:x 0, :y 1}, :input-coord {:x 1, :y 1}, :distance 1, :closest-input-coord {:x 1, :y 1}, :boundary true}
                     nil
                     {:grid-coord {:x 1, :y 0}, :input-coord {:x 1, :y 1}, :distance 1, :closest-input-coord {:x 1, :y 1}, :boundary true}
                     {:grid-coord {:x 1, :y 1}, :input-coord {:x 1, :y 1}, :distance 0, :closest-input-coord {:x 1, :y 1}, :boundary false}
                     nil
                     nil
                     nil
                     {:grid-coord {:x 2, :y 2}, :input-coord {:x 2, :y 2}, :distance 0, :closest-input-coord {:x 2, :y 2}, :boundary true}))))}
  [{:keys [x-min x-max y-min y-max] :as boundary} input-coords]
  (for [x (range x-min (inc x-max))
        y (range y-min (inc y-max))]
    (let [grid-coord {:x x :y y}
          grid-closest-input-coord (find-closest-input-point grid-coord input-coords)]
      (when grid-closest-input-coord
        (assoc grid-closest-input-coord :boundary (boundary? grid-coord boundary))))))
;; (test #'find-each-closest-input-point)
;; (find-each-closest-input-point {:x-min 0 :x-max 2 :y-min 0 :y-max 2} '({:x 1 :y 1} {:x 2 :y 2}))
;; 이름을 직관적으로 변경
;; find-each-closest-input-point

(defn part-1 []
  (let [input-coords (->> (read-file 2018 6)
                          (map coordinate-str))
        boundary (extract-boundary input-coords)
        each-grid-with-closest (find-each-closest-input-point boundary input-coords)
        boundary-input-coords (->> (filter #(:boundary %) each-grid-with-closest)
                                   (group-by :closest-input-coord)
                                   keys)]
    (->> each-grid-with-closest
         (remove (fn [grid-with-closest]
                   ((set boundary-input-coords) (:closest-input-coord grid-with-closest))))
         (map :closest-input-coord)
         frequencies
         (sort-by val >)
         first)))


(defn calc-each-grid-sum-of-distance
  "각 좌표에서 각 포인트까지의 거리의 합"
  {:test
   #(do (assert (= (calc-each-grid-sum-of-distance {:x-min 0 :x-max 4 :y-min 0 :y-max 4} '({:x 1 :y 2} {:x 3 :y 4}))
                   '(10 8 6 6 6 8 6 4 4 4 8 6 4 4 4 8 6 4 4 4 10 8 6 6 6))))}
  [{:keys [x-min x-max y-min y-max] :as boundary} input-coords]
  (for [x (range x-min (inc x-max))
        y (range y-min (inc y-max))]
    (let [grid-coord {:x x :y y}]
      (->> (map #(calc-manhattan-distance grid-coord %) input-coords)
           (apply +)))))
;; (test #'calc-sum-of-distance)
;; (coord-distance-sum {:x-min 0 :x-max 4 :y-min 0 :y-max 4} '({:x 1 :y 2} {:x 3 :y 4}))

(defn part-2 []
  (let [input-coords (->> (read-file 2018 6)
                          (map coordinate-str))
        boundary (extract-boundary input-coords)]
    (->> (calc-each-grid-sum-of-distance boundary input-coords)
         (filter #(> 10000 %))
         count)))

(comment
  (part-1)
  (part-2)

  (read-file 2018 6 :type :sample)
  (+ 1 2)
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