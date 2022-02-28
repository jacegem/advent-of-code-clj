(ns advent-of-code-clj.2018.day-04
  (:require [clojure.string :as string]
            [clojure.repl :refer [doc]]))

;; https://adventofcode.com/2018/day/4
;; --- Day 4: Repose Record ---

;; [1518-11-11 00:04] Guard #2179 begins shift
;; [1518-09-15 00:38] wakes up
;; [1518-10-19 00:22] wakes up
;; [1518-08-14 00:45] falls asleep
;; [1518-10-16 00:47] falls asleep
;; [1518-10-27 00:02] Guard #3181 begins shift
;;
;; record -> 일반적이름

(defn read-file [year day]
  (->
   (format "resources/%d/input_%02d.txt" year day)
   slurp
   string/split-lines))

(defn parse-int [guard-id]
  (if (nil? guard-id)
    nil
    (Integer/parseInt guard-id)))

(defn parse-int-2 [guard-id]
  (and guard-id (Integer/parseInt guard-id)))
;; good



(defn parse-record [record]
  (let [[year month day hour minute] (->> (re-seq #"\d+" record)
                                          (map #(Integer/parseInt %)))
        id  (->> (re-find #"(?<=#)\d+" record)
                 parse-int)
        activity  (->> (re-find #"shift|asleep|wakes" record)
                       keyword)]
    {:year year
     :month month
     :day day
     :hour hour
     :minute minute
     :id id
     :activity activity}))
;; 정규표현식 한번에 사용

(defn reduce-record [{data :data guard :guard asleep :asleep}
                     {activity :activity id :id :as record}]
  (case activity
    :shift {:data (if (contains? data id)
                    data
                    (assoc data id []))
            :guard id}
    :asleep {:data data
             :guard guard
             :asleep record}
    :wakes {:data (update data guard #(conj % [asleep record]))
            :guard guard}
    :default))
;; record - 일반적,
;; reduce - 일반적, => 집계한다 -> 도메인을 담아서, 경비원의 활동 기록을 원하는 데이터형태로 정리, 작업을 준비 prepare guard activity
;; 이름이 잘 안지어지면, 함수가 선언적이지 않다. 도메인에 안 맞는 것일 수 있다.
;; { 123 [[1 2] [3 4]] 444 [[4 5] [5 6]]}
;; { 123 { guard-id 123, sleep [[1 2]] } }

(defn asleep-time [[guard datas]]
  {:guard guard
   :time (->>
          (map (fn [[start end]]
                 (- (:minute end) (:minute start))) datas)
          (apply +))})

;; most asleep
(defn most-asleep [data]
  (->>
   (map asleep-time data)
   (sort-by :time)
   reverse
   first
   :guard))

;; most minute
(defn most-minute [data]
  (->>
   (mapcat
    (fn [[start end]]
      (range (:minute start) (inc (:minute end))))
    data)
   frequencies
   (sort-by val)
   reverse
   first
   first))

(defn part-1 []
  (let [data (->> (read-file 2018 4)
                  sort
                  (map parse-record)
                  (reduce reduce-record {})
                  :data)
        guard-id (most-asleep data)
        minute (-> (get data guard-id) most-minute)]
    (* guard-id minute)))


;; guard-id frequently asleep on the same minute
;; [[minute id] [minute id] [minute id]] -> freq

(defn guard-id-minute [[guard-id history]]
  (mapcat
   (fn [[start end]]
     (for [minute (range (:minute start) (inc (:minute end)))]
       [guard-id minute]))
   history))

(defn part-2 []
  (let [data (->> (read-file 2018 4)
                  sort
                  (map parse-record)
                  (reduce reduce-record {})
                  :data)
        [guard-id minute] (->> data
                               (mapcat guard-id-minute)
                               frequencies
                               (sort-by val)
                               reverse
                               second ; [[3331 42] 17] [[3331 41] 17]
                               first)]
    (* guard-id minute)))


(comment
  (part-1)
  (part-2)

  (->> (read-file 2018 4)
       sort
       (map parse-record)
       (reduce reduce-record {})
       :data

      ;;  (map most-freq-minute)
       )
  ;=> {2671 [[{:year 1518, :month 3, :day 21, :hour 0, :minute 11, :activity :asleep} {:year 1518, :month 3, :day 21, :hour 0, :minute 20, :activity :wakes}] [{:year 1518, :month 3, :day 21, :hour 0, :minute 25, :activity :asleep} {:year 1518, :month 3, :day 21, :hour 0, :minute 58, :activity :wakes}] [{:year 1518, :month 4, :day 26, :hour 0, :minute 6, :activity :asleep} {:year 1518, :month 4, :day 26, :hour 0, :minute 14, :activity :wakes}] [{:year 1518, :month 4, :day 26, :hour 0, :minute 33, :activity :asleep} {:year 1518, :month 4, :day 26, :hour 0, :minute 47, :activity :wakes}] [{:year 1518, :month 4, :day 26, :hour 0, :minute 53, :activity :asleep} {:year 1518, :month 4, :day 26, :hour 0, :minute 54, :activity :wakes}] [{:year 1518, :month 5, :day 14, :hour 0, :minute 9, :activity :asleep} {:year 1518, :month 5, :day 14, :hour 0, :minute 36, :activity :wakes}] [{:year 1518, :month 6, :day 19, :hour 0, :minute 5, :activity :asleep} {:year 1518, :month 6, :day 19, :hour 0, :minute 42, :activity :wakes}] [{:year 1518, :month 7, :day 3, :hour 0, :minute 21, :activity :asleep} {:year 1518, :month 7, :day 3, :hour 0, :minute 53, :activity :wakes}] [{:year 1518, :month 7, :day 6, :hour 0, :minute 8, :activity :asleep} {:year 1518, :month 7, :day 6, :hour 0, :minute 23, :activity :wakes}] [{:year 1518, :month 7, :day 6, :hour 0, :minute 29, :activity :asleep} {:year 1518, :month 7, :day 6, :hour 0, :minute 55, :activity :wakes}] [{:year 1518, :month 8, :day 8, :hour 0, :minute 4, :activity :asleep} {:year 1518, :month 8, :day 8, :hour 0, :minute 19, :activity :wakes}] [{:year 1518, :month 8, :day 8, :hour 0, :minute 45, :activity :asleep} {:year 1518, :month 8, :day 8, :hour 0, :minute 59, :activity :wakes}] [{:year 1518, :month 8, :day 24, :hour 0, :minute 9, :activity :asleep} {:year 1518, :month 8, :day 24, :hour 0, :minute 35, :activity :wakes}] [{:year 1518, :month 8, :day 24, :hour 0, :minute 52, :activity :asleep} {:year 1518, :month 8, :day 24, :hour 0, :minute 58, :activity :wakes}] [{:year 1518, :month 11, :day 3, :hour 0, :minute 32, :activity :asleep} {:year 1518, :month 11, :day 3, :hour 0, :minute 40, :activity :wakes}]], 3251 [[{:year 1518, :month 3, :day 29, :hour 0, :minute 6, :activity :asleep} {:year 1518, :month 3, :day 29, :hour 0, :minute 7, :activity :wakes}] [{:year 1518, :month 3, :day 29, :hour 0, :minute 36, :activity :asleep} {:year 1518, :month 3, :day 29, :hour 0, :minute 45, :activity :wakes}] [{:year 1518, :month 4, :day 6, :hour 0, :minute 21, :activity :asleep} {:year 1518, :month 4, :day 6, :hour 0, :minute 51, :activity :wakes}] [{:year 1518, :month 4, :day 15, :hour 0, :minute 14, :activity :asleep} {:year 1518, :month 4, :day 15, :hour 0, :minute 18, :activity :wakes}] [{:year 1518, :month 4, :day 15, :hour 0, :minute 30, :activity :asleep} {:year 1518, :month 4, :day 15, :hour 0, :minute 48, :activity :wakes}] [{:year 1518, :month 5, :day 4, :hour 0, :minute 0, :activity :asleep} {:year 1518, :month 5, :day 4, :hour 0, :minute 23, :activity :wakes}] [{:year 1518, :month 5, :day 4, :hour 0, :minute 48, :activity :asleep} {:year 1518, :month 5, :day 4, :hour 0, :minute 56, :activity :wakes}] [{:year 1518, :month 5, :day 20, :hour 0, :minute 6, :activity :asleep} {:year 1518, :month 5, :day 20, :hour 0, :minute 28, :activity :wakes}] [{:year 1518, :month 6, :day 12, :hour 0, :minute 1, :activity :asleep} {:year 1518, :month 6, :day 12, :hour 0, :minute 14, :activity :wakes}] [{:year 1518, :month 6, :day 12, :hour 0, :minute 24, :activity :asleep} {:year 1518, :month 6, :day 12, :hour 0, :minute 50, :activity :wakes}] [{:year 1518, :month 7, :day 7, :hour 0, :minute 11, :activity :asleep} {:year 1518, :month 7, :day 7, :hour 0, :minute 29, :activity :wakes}] [{:year 1518, :month 8, :day 31, :hour 0, :minute 2, :activity :asleep} {:year 1518, :month 8, :day 31, :hour 0, :minute 35, :activity :wakes}] [{:year 1518, :month 9, :day 14, :hour 0, :minute 5, :activity :asleep} {:year 1518, :month 9, :day 14, :hour 0, :minute 10, :activity :wakes}] [{:year 1518, :month 9, :day 14, :hour 0, :minute 14, :activity :asleep} {:year 1518, :month 9, :day 14, :hour 0, :minute 37, :activity :wakes}] [{:year 1518, :month 9, :day 14, :hour 0, :minute 50, :activity :asleep} {:year 1518, :month 9, :day 14, :hour 0, :minute 53, :activity :wakes}] [{:year 1518, :month 9, :day 15, :hour 0, :minute 11, :activity :asleep} {:year 1518, :month 9, :day 15, :hour 0, :minute 38, :activity :wakes}]], 3181 [[{:year 1518, :month 3, :day 4, :hour 0, :minute 22, :activity :asleep} {:year 1518, :month 3, :day 4, :hour 0, :minute 38, :activity :wakes}] [{:year 1518, :month 3, :day 4, :hour 0, :minute 43, :activity :asleep} {:year 1518, :month 3, :day 4, :hour 0, :minute 57, :activity :wakes}] [{:year 1518, :month 4, :day 8, :hour 0, :minute 5, :activity :asleep} {:year 1518, :month 4, :day 8, :hour 0, :minute 6, :activity :wakes}] [{:year 1518, :month 4, :day 8, :hour 0, :minute 32, :activity :asleep} {:year 1518, :month 4, :day 8, :hour 0, :minute 54, :activity :wakes}] [{:year 1518, :month 5, :day 18, :hour 0, :minute 26, :activity :asleep} {:year 1518, :month 5, :day 18, :hour 0, :minute 36, :activity :wakes}] [{:year 1518, :month 5, :day 18, :hour 0, :minute 40, :activity :asleep} {:year 1518, :month 5, :day 18, :hour 0, :minute 57, :activity :wakes}] [{:year 1518, :month 7, :day 8, :hour 0, :minute 29, :activity :asleep} {:year 1518, :month 7, :day 8, :hour 0, :minute 31, :activity :wakes}] [{:year 1518, :month 7, :day 8, :hour 0, :minute 38, :activity :asleep} {:year 1518, :month 7, :day 8, :hour 0, :minute 40, :activity :wakes}] [{:year 1518, :month 7, :day 8, :hour 0, :minute 43, :activity :asleep} {:year 1518, :month 7, :day 8, :hour 0, :minute 46, :activity :wakes}] [{:year 1518, :month 7, :day 9, :hour 0, :minute 24, :activity :asleep} {:year 1518, :month 7, :day 9, :hour 0, :minute 39, :activity :wakes}] [{:year 1518, :month 7, :day 26, :hour 0, :minute 26, :activity :asleep} {:year 1518, :month 7, :day 26, :hour 0, :minute 48, :activity :wakes}] [{:year 1518, :month 8, :day 16, :hour 0, :minute 16, :activity :asleep} {:year 1518, :month 8, :day 16, :hour 0, :minute 50, :activity :wakes}] [{:year 1518, :month 8, :day 26, :hour 0, :minute 33, :activity :asleep} {:year 1518, :month 8, :day 26, :hour 0, :minute 43, :activity :wakes}] [{:year 1518, :month 8, :day 26, :hour 0, :minute 49, :activity :asleep} {:year 1518, :month 8, :day 26, :hour 0, :minute 50, :activity :wakes}] [{:year 1518, :month 8, :day 26, :hour 0, :minute 53, :activity :asleep} {:year 1518, :month 8, :day 26, :hour 0, :minute 58, :activity :wakes}] [{:year 1518, :month 9, :day 19, :hour 0, :minute 3, :activity :asleep} {:year 1518, :month 9, :day 19, :hour 0, :minute 42, :activity :wakes}] [{:year 1518, :month 9, :day 24, :hour 0, :minute 21, :activity :asleep} {:year 1518, :month 9, :day 24, :hour 0, :minute 52, :activity :wakes}] [{:year 1518, :month 10, :day 27, :hour 0, :minute 10, :activity :asleep} {:year 1518, :month 10, :day 27, :hour 0, :minute 25, :activity :wakes}] [{:year 1518, :month 11, :day 19, :hour 0, :minute 36, :activity :asleep} {:year 1518, :month 11, :day 19, :hour 0, :minute 56, :activity :wakes}] [{:year 1518, :month 11, :day 21, :hour 0, :minute 2, :activity :asleep} {:year 1518, :month 11, :day 21, :hour 0, :minute 58, :activity :wakes}] [{:year 1518, :month 11, :day 22, :hour 0, :minute 0, :activity :asleep} {:year 1518, :month 11, :day 22, :hour 0, :minute 45, :activity :wakes}] [{:year 1518, :month 11, :day 22, :hour 0, :minute 52, :activity :asleep} {:year 1518, :month 11, :day 22, :hour 0, :minute 53, :activity :wakes}]], 311 [[{:year 1518, :month 3, :day 3, :hour 0, :minute 24, :activity :asleep} {:year 1518, :month 3, :day 3, :hour 0, :minute 55, :activity :wakes}] [{:year 1518, :month 3, :day 31, :hour 0, :minute 44, :activity :asleep} {:year 1518, :month 3, :day 31, :hour 0, :minute 46, :activity :wakes}] [{:year 1518, :month 3, :day 31, :hour 0, :minute 51, :activity :asleep} {:year 1518, :month 3, :day 31, :hour 0, :minute 53, :activity :wakes}] [{:year 1518, :month 3, :day 31, :hour 0, :minute 57, :activity :asleep} {:year 1518, :month 3, :day 31, :hour 0, :minute 58, :activity :wakes}] [{:year 1518, :month 5, :day 3, :hour 0, :minute 5, :activity :asleep} {:year 1518, :month 5, :day 3, :hour 0, :minute 45, :activity :wakes}] [{:year 1518, :month 5, :day 3, :hour 0, :minute 52, :activity :asleep} {:year 1518, :month 5, :day 3, :hour 0, :minute 54, :activity :wakes}] [{:year 1518, :month 5, :day 29, :hour 0, :minute 15, :activity :asleep} {:year 1518, :month 5, :day 29, :hour 0, :minute 42, :activity :wakes}] [{:year 1518, :month 5, :day 29, :hour 0, :minute 53, :activity :asleep} {:year 1518, :month 5, :day 29, :hour 0, :minute 58, :activity :wakes}] [{:year 1518, :month 6, :day 1, :hour 0, :minute 29, :activity :asleep} {:year 1518, :month 6, :day 1, :hour 0, :minute 30, :activity :wakes}] [{:year 1518, :month 6, :day 1, :hour 0, :minute 40, :activity :asleep} {:year 1518, :month 6, :day 1, :hour 0, :minute 48, :activity :wakes}] [{:year 1518, :month 6, :day 1, :hour 0, :minute 53, :activity :asleep} {:year 1518, :month 6, :day 1, :hour 0, :minute 54, :activity :wakes}] [{:year 1518, :month 6, :day 18, :hour 0, :minute 14, :activity :asleep} {:year 1518, :month 6, :day 18, :hour 0, :minute 33, :activity :wakes}] [{:year 1518, :month 7, :day 14, :hour 0, :minute 0, :activity :asleep} {:year 1518, :month 7, :day 14, :hour 0, :minute 39, :activity :wakes}] [{:year 1518, :month 7, :day 29, :hour 0, :minute 24, :activity :asleep} {:year 1518, :month 7, :day 29, :hour 0, :minute 43, :activity :wakes}] [{:year 1518, :month 8, :day 12, :hour 0, :minute 35, :activity :asleep} {:year 1518, :month 8, :day 12, :hour 0, :minute 39, :activity :wakes}] [{:year 1518, :month 8, :day 12, :hour 0, :minute 50, :activity :asleep} {:year 1518, :month 8, :day 12, :hour 0, :minute 52, :activity :wakes}] [{:year 1518,



  (defn guard-id-minute [[guard-id history]]
    (mapcat
     (fn [[start end]]
       (for [minute (range (:minute start) (inc (:minute end)))]
         [guard-id minute]))
     history))

  (for [x (range 1 4)] ["X" x])

  ;; (defn most-freq-minute [data]
  ;;   (->>
  ;;    (map guard-id-minute data)))

  (def data (->> (read-file 2018 4)
                 sort
                 (map parse-record)
                 (reduce reduce-record {})
                 :data))

  (->> data
       (mapcat guard-id-minute)
       frequencies
       (sort-by val)
       reverse
       second ; [3331 42] 17] [[3331 41] 17]
       first)

  (* 3331 42)

  ;; (most-freq-minute data)

  data



;;  {1 {[2 1] [3 2] [4 3]}}  
;;  [1분 1번 2번]

  (defn read-file [year day]
    (->
     (format "resources/%d/input_%02d.txt" year day)
     slurp
     string/split-lines))
  (read-file 2018 4)



;; [1518-11-11 00:04] Guard #2179 begins shift
;; [1518-09-15 00:38] wakes up
;; [1518-10-19 00:22] wakes up
;; [1518-08-14 00:45] falls asleep
;; [1518-10-16 00:47] falls asleep
;; [1518-10-27 00:02] Guard #3181 begins shift

   ;; parse 
  (def input (read-file 2018 4))
  (def sorted-record (sort input))
  (map println sorted-record)


  (re-seq #"\#\d+" (first input))

  (re-find #"(?<=#)\d+" "#999")
  (->
   (re-seq #"(?<=#)\d+" "999")
   #(Integer/parseInt %))
  (Integer/parseInt nil)

  ((fn [id] (when-not (nil? id) (Integer/parseInt id))) "999")
  (->> (re-find #"(?<=#)\d+" "[1518-11-11 00:04] Guard #2179 begins shift")
       (fn [gid] (when-not (nil? gid) (Integer/parseInt gid))))


  (defn parse-int [guard-id]
    (if (nil? guard-id)
      nil
      (Integer/parseInt guard-id)))

  (defn parse-record [record]
    (let [[year month day hour minute] (->> (re-seq #"\d+" record)
                                            (map #(Integer/parseInt %)))
          id  (->> (re-find #"(?<=#)\d+" record)
                   parse-int)
          activity  (->> (re-find #"shift|asleep|wakes" record)
                         keyword)]
      {:year year
       :month month
       :day day
       :hour hour
       :minute minute
       :id id
       :activity activity}))

  (def parsed-records (map parse-record sorted-record))
  parsed-records

  (parse-record "[1518-11-11 00:04] Guard #2179 begins shift")
  ;=> {:year 1518, :month 11, :day 11, :hour 0, :minute 4, :id "#2179", :activity :begins}

  (def parsed-records (map parse-record sorted-record))
  ;=> ({:year 1518, :month 8, :day 8, :hour 0, :minute 45, :id nil, :activity :asleep} {:year 1518, :month 5, :day 2, :hour 0, :minute 52, :id nil, :activity :asleep} 
  parsed-records


  (defn reduce-record [{data :data guard :guard asleep :asleep}
                       {activity :activity id :id :as record}]
    (case activity
      :shift {:data (if (contains? data id)
                      data
                      (assoc data id []))
              :guard id}
      :asleep {:data data
               :guard guard
               :asleep (dissoc record :id)}
      :wakes {:data (update data guard #(conj % [asleep (dissoc record :id)]))
              :guard guard}
      :default))

  (defn asleep-time [[guard datas]]
    {:guard guard
     :time (->>
            (map (fn [[start end]] (- (:minute end) (:minute start))) datas)
            (apply +))})

  ;; most asleep
  (defn most-asleep [data]
    (->>
     (map asleep-time data)
     (sort-by :time)
     reverse
     first
     :guard))

  ;; most minute
  (defn most-minute [data]
    (->>
     (mapcat
      (fn [[start end]] (range (:minute start) (inc (:minute end))))
      data)
     frequencies
     (sort-by val)
     reverse
     first
     first))




  (let [data (->> (reduce reduce-record {} parsed-records)
                  :data)
        guard-id (most-asleep data)
        minute (-> (get data guard-id) most-minute)]
    (println guard-id)
    (println minute)
    (* guard-id minute))


  (->> (reduce reduce-record {} parsed-records)
       :data
       most-asleep)
  ;=> 1021
  (def data (->> (reduce reduce-record {} parsed-records)
                 :data))

  (def guard-data (get data 1021))
  (map (fn [[start end]] range ((:minute start) (inc (:minute end))) guard-data))



  (let [data (->> (reduce reduce-record {} parsed-records)
                  :data
                  (map asleep-time)
                  (sort-by :time)
                  reverse
                  first)
        guard (:guard data)
        time (:time data)]

    (* guard time))







  (defn get-schedule [[acc begins asleep wakes] record])

  (reduce #() [acc begins asleep wakes] parsed-records)
  ;; activity 가 begins, asleep, wakes

  (def example '("[1518-11-01 00:00] Guard #10 begins shift", "[1518-11-01 00:05] falls asleep", "[1518-11-01 00:25] wakes up", "[1518-11-01 00:30] falls asleep", "[1518-11-01 00:55] wakes up", "[1518-11-01 23:58] Guard #99 begins shift", "[1518-11-02 00:40] falls asleep", "[1518-11-02 00:50] wakes up", "[1518-11-03 00:05] Guard #10 begins shift", "[1518-11-03 00:24] falls asleep", "[1518-11-03 00:29] wakes up", "[1518-11-04 00:02] Guard #99 begins shift", "[1518-11-04 00:36] falls asleep", "[1518-11-04 00:46] wakes up", "[1518-11-05 00:03] Guard #99 begins shift", "[1518-11-05 00:45] falls asleep", "[1518-11-05 00:55] wakes up"))
  example

  ;; 최장시간 구하기 {"#2671" 240, }

  (map (fn [[k v]] (println k v)) {:a "aa" :b "bb"})

  (reduce reduce-record {} (take 3 parsed-records))

  (first parsed-records)

  (assoc-in {} ["a"] [])

  ; stacktrace
  ; 손호성_신선마켓개발

  ; 포탈 디버깅 툴, 외부 툴 활용
  ; mate clj

  ; map, filter 로 대체 가능한가?
  ; range (0~12), 
  ; loop, recur, reduce

  (and (nil? "nil") (println "A"))

;; {:id []}

;; https://github.com/mattiasl/aoc2018-clojure/blob/master/src/aoc2018/day_04.clj
;; https://github.com/lfsmoura/AoC2018/blob/master/p04/a.clj
;; https://github.com/joinr/aoc2018/blob/master/src/aoc2018/four.clj
;; https://gist.github.com/ynonp/701cbadca2ed513b7f39ab0b7e37c934  
  '())

