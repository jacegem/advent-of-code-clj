(ns advent-of-code-clj.2018.day-04-v2
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]))


;; https://adventofcode.com/2018/day/4
;; --- Day 4: Repose Record ---

;; input
;; [1518-11-11 00:04] Guard #2179 begins shift
;; [1518-09-15 00:38] wakes up
;; [1518-10-19 00:22] wakes up
;; [1518-08-14 00:45] falls asleep
;; [1518-10-16 00:47] falls asleep
;; [1518-10-27 00:02] Guard #3181 begins shift

(s/def ::guard-id int?)
(s/def ::minutes (s/coll-of int?))
(s/def ::event (s/keys :req [::guard-id ::year ::month ::day ::hour ::minute ::activity]))

(defn read-file [year day]
  (->
   (format "resources/%d/input_%02d.txt" year day)
   slurp
   string/split-lines))

(defn parse-int
  "문자열을 숫자로 변환"
  {:test (do #(assert (= (parse-int nil) nil))
             #(assert (= (parse-int "1") 1))
             #(assert (= (parse-int "23") 23)))}
  [guard-id]
  (if (nil? guard-id)
    nil
    (Integer/parseInt guard-id)))

(s/fdef input->event
  :args (s/cat :input string?)
  :ret ::event)

(defn input->event
  "input -> event"
  {:test
   (do
     #(assert (= (input->event "[1518-11-11 00:04] Guard #2179 begins shift")
                 {:year 1518, :month 11, :day 11, :hour 0, :minute 4, :guard-id 2179, :activity :shift}))
     #(assert (= (input->event "[1518-09-15 00:38] wakes up")
                 {:year 1518, :month 9, :day 15, :hour 0, :minute 38, :guard-id nil, :activity :up}))
     #(assert  (= (input->event "[1518-08-14 00:45] falls asleep")
                  {:year 1518, :month 8, :day 14, :hour 0, :minute 45, :guard-id nil, :activity :asleep})))}
  [input]
  (let [[_ year month day hour minute guard-id activity]
        (re-matches #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (?:(?:Guard #)(\d+))?.*(shift|up|asleep)" input)]
    {:year (parse-int year)
     :month (parse-int month)
     :day (parse-int day)
     :hour (parse-int hour)
     :minute (parse-int minute)
     :guard-id (parse-int guard-id)
     :activity (keyword activity)}))

(defn generate-gaurd-sleep-event [{data :data guard :guard sleep-event :sleep-event}
                                  {activity :activity guard-id :guard-id :as event}]
  (case activity
    :shift {:data (if (contains? data guard-id)
                    data
                    (assoc data guard-id []))
            :guard guard-id}
    :asleep {:data data
             :sleep-event event
             :guard guard}
    :up {:data (update data guard #(conj % [sleep-event event]))
         :guard guard}
    :default))

(defn sleep-range
  {:test
   #(assert (= (sleep-range [631 [[{:year 1518, :month 2, :day 11, :hour 0, :minute 3, :guard-id nil, :activity :asleep} {:year 1518, :month 2, :day 11, :hour 0, :minute 19, :guard-id nil, :activity :up}]]])
               {:guard-id 631, :count 17, :minutes '(3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)}))}
  [[guard-id events]]
  (let [minutes (mapcat (fn [[{start :minute} {end :minute}]]
                          (range start (inc end))) events)
        count (count minutes)]
    {:guard-id guard-id
     :minutes minutes
     :count count}))

(defn read-input []
  (->> (read-file 2018 4)
       sort
       (map input->event)
       (reduce generate-gaurd-sleep-event {})
       :data
       (map sleep-range)))

(defn get-most-sleep
  {:test
   (do #(assert (= (get-most-sleep '({:guard-id 1 :count 1} {:guard-id 2 :count 2}))
                   {:guard-id 2 :count 2}))
       #(assert (= (get-most-sleep '({:guard-id 3 :count 3} {:guard-id 2 :count 2}))
                   {:guard-id 3 :count 3})))}
  [data]
  (->> data
       (sort-by :count)
       reverse
       first))

(defn get-most-minute
  {:test
   #(assert (= (get-most-minute '(11 12 13 14 15 16 13 14 15 15 16))
               15))}
  [minutes]
  (->> (frequencies minutes)
       (sort-by val)
       reverse
       first
       first))

(defn part-1 []
  (let [data (read-input)
        most-sleep (get-most-sleep data)
        most-minute (get-most-minute (:minutes most-sleep))]
    (* (:guard-id most-sleep) most-minute)))




(comment
  (part-1)
  (do
    (test #'parse-int)
    (test #'input->event)
    (test #'sleep-range)
    (test #'get-most-minute)
    (test #'get-most-sleep))
  (input->event "[1518-11-11 00:04] Guard #2179 begins shift")
  (input->event "[1518-09-15 00:38] wakes up")
  (input->event "[1518-08-14 00:45] falls asleep")


  (require '[clojure.spec.test.alpha :as stest])
  (stest/instrument)

  (->> (read-file 2018 4)
       sort
       (map input->event)
       (reduce generate-gaurd-sleep-event {})
       :data
       (map sleep-range)
       (sort-by :count)
       reverse
       first
       :count)

  (def data (read-input))
  (:minutes (first (take 1 data)))


  (sleep-range [631 [[{:year 1518, :month 2, :day 11, :hour 0, :minute 3, :guard-id nil, :activity :asleep} {:year 1518, :month 2, :day 11, :hour 0, :minute 19, :guard-id nil, :activity :up}]]])

  (def datas
    [{:guard-id 123, :sleep-event [["event" "event"]]}
     {:guard-id 456, :sleep-event [["event" "event"]]}])

  (filter #(= 123 (:guard-id %)) datas)
  (first
   (filter (fn [data] (= 1234 (:guard-id data))) datas))

  '())




;; guard-sleep-event
;; [
;;  {:guard-id 123, :sleep-event [[event event] [event event] [event event]]}
;;  {:guard-id 456, :sleep-event [[event event] [event event] [event event]]} 
;;]
;; (s/def ::sleep-event (s/coll-of (s/coll-of ::event)))
;; (s/def ::guard-sleep-event
;;   (s/keys :req [::guard-id ::sleep-event]))
