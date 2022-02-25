(ns advent-of-code-clj.2018.day-03
  (:require [clojure.string :as string]))

;; https://adventofcode.com/2018/day/3
;; --- Day 3: No Matter How You Slice It ---

(defn read-file []
  (->
   (slurp "resources/2018/input_03.txt")
   string/split-lines))

(defn match-data [s]
  (map #(Integer/parseInt %)
       (rest
        (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" s))))
;; 스레드 매크로 사용 (가독성 향상)
;; next 함수 권장, rest 성능 이슈 확인?
;; 자바 함수 이므로 익명함수로 감싸야 한다.

(defn parse-data [s]
  (let [[id x y w h] (match-data s)]
    {:id id
     :x x
     :y y
     :w w
     :h h}))
;; 컴마 사용 안하는 것이 기본, 한 줄인 경우에 컴마 사용

(defn get-square [m]
  (for [x (range (:x m) (+ (:x m) (:w m)))
        y (range (:y m) (+ (:y m) (:h m)))]
    [x y]))
;; keys? 

(defn get-square [{x :x y :y}  m]
  (for [x (range (:x m) (+ (:x m) (:w m)))
        y (range (:y m) (+ (:y m) (:h m)))]
    [x y]))

(defn get-square [{:keys [x y w h]} m]
  (for [my-x (range x (+ x w))
        my-y (range y (+ y h))]
    [my-x my-y]))

(defn parse-claim [claim]
  (->> claim
       parse-data
       get-square))

(defn part-1 []
  (->>
   (mapcat parse-claim (read-file))
  ;;  (apply concat)
   frequencies
   (filter (fn [[_ v]] (> v 1)))
   count))
;; 익명 함수 안 만들어도 됨
;; mapcat 사용 -> map - cat

(part-1)

(defn overlap? [map-a map-b]
  (if (= (:id map-a) (:id map-b))
    false
    (let [square-a (get-square map-a)
          square-b (get-square map-b)]
      (->>
       (concat square-a square-b)
       frequencies
       (filter (fn [[_ v]] (> v 1)))
       seq
       boolean))))
;; when-not 사용 가능, nil 반환
;; 파라미터 이름 명확하게 작성
;; 구현체 기준이 아니라, 추상을 기준으로 명명한다. 
;; some 사용??

;; (def not-exist-data {:id -99 :x -99 :y -99 :w -99 :h -99})

(defn not-overlap [patch-list]
  (->>
   (for [data patch-list]
     (when (every? #(not (overlap? data %)) patch-list)
       data))
   (filter identity)
   first))
;; O^2
;; lazy-seq 활용
;; 이름, 명확하게
;; defrecord, defstruct

(defn part-2 []
  (->> (map parse-data (read-file))
       not-overlap))

(time (part-2))


(comment
    ;; #1 @ 1,3: 4x4
    ;; x,y 좌표로 처리
    ;; #1 @ 1,3: 4x4 => x: 1~5 (1,2,3,4), y: 3~7 (3,4,5,6)
  ;; PPAP
  (defn read-file []
    (->
     (slurp "resources/2018/input_03.txt")
     string/split-lines))


  (def input (read-file))
  (def m (map parse-data input))
  m

  (defn overlap? [ma mb]
    (if (= (:id ma) (:id mb))
      false
      (let [square-a (get-square ma)
            square-b (get-square mb)]
        (->>
         (concat square-a square-b)
         frequencies
         (filter (fn [[_ v]] (> v 1)))
         seq
         boolean))))

  (for [data m]
    (when (every? #(not (overlap? data %)) m)
      (println data)))

  (+ 1 2)


  (defn part-2 []
    (->>
     (map #(parse-claim %) (read-file))
     (apply concat)
     frequencies
     (filter (fn [[_ v]] (> v 1)))))

  (map #(meta %)
       (part-2))


  (read-file)
  ;; overlap?
  ;; 오버랩 되지 않는 것을 찾는다.
  ;; all ids

  ; "#1124 @ 888,828: 25x24" "#1125 @ 348,461: 19x25"  



  (defn get-square-by-claim [s]
    (->> (parse-claim s)
         get-square))


  (defn overlap? [sa sb]
    (if (= (:id (parse-data sa)) (:id (parse-data sb)))
      false
      (let [square-a (get-square-by-claim sa)
            square-b (get-square-by-claim sb)]
      ;; (println square-a square-b)
        (->>
         (concat square-a square-b)
         frequencies
         (filter (fn [[_ v]] (> v 1)))
         seq
         boolean))))


  (overlap? "#1124 @ 888,828: 25x24" "#1125 @ 348,461: 19x25")
  (overlap? "#1124 @ 888,828: 25x24" "#1125 @ 888,828: 25x24")

  (:id
   (parse-data "#1124 @ 888,828: 25x24"))






  (def input (read-file))
  (first input)
  (second input)

  (overlap? (first input) (second input))


  (def short
    (take 10 input))


  (every? #(= true %) '(true true false))


  (defn not-overlap? [claim xs]
    (every? #(not (overlap? claim %)) xs))


  (for [claim short]
    (when (not-overlap? claim short)
      (println claim)))

  (for [claim-a short
        claim-b short
        :when (not (overlap? claim-a claim-b))]
    [claim-a])




  input

  (let [input (read-file)]
    (for [claim-a [input] claim-b [input]
          :when (not (overlap? claim-a claim-b))]
      (println claim-a)))

  (for [a (range 3) b (range 10 14)] [a b])

  (defn match-data [s]
    (map #(Integer/parseInt %)
         (rest
          (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" s))))

  (defn parse-claim [s]
    (let [[id x y w h] (match-data s)]
      {:id id,
       :x x,
       :y y,
       :w w,
       :h h}))

  (defn get-square [m]
    (for [x (range (:x m) (+ (:x m) (:w m)))
          y (range (:y m) (+ (:y m) (:h m)))]
      [x y]))

  (defn parse-claim [claim]
    (->> claim
         parse-claim
         get-square))


  (->>
   (apply concat
          (map #(parse-claim %) (read-file)))
   frequencies
   (filter (fn [[_ v]] (> v 1)))
   seq)



  (def l
    '(([1 2] [3 4]) ([1 2] [4 5])))
  ;=> ([1 2] [3 4] [1 2] [4 5])

  (apply concat l)
  (into l)


  (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" "#1 @ 1,3: 4x4")
  (parse-claim "#1 @ 1,3: 4x4")

  (def m
    {:id 1, :x 1, :y 3, :w 4, :h 4})

  (for [x (range (:x m) (+ (:x m) (:w m)))
        y (range (:y m) (+ (:y m) (:h m)))]
    [x y])

  (frequencies
   (for [x (range (:x m) (+ (:x m) (:w m)))
         y (range (:y m) (+ (:y m) (:h m)))]
     [x y]))

  (for [x (range 1 3) y (range 4 6)]
    [x y])

  (defn positions [m])

  (defn parse-claim [s])


  '())