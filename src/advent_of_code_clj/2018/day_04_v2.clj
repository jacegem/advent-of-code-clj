(ns advent-of-code-clj.2018.day-04-v2
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.spec.test.alpha :as stest]))


;; https://adventofcode.com/2018/day/4
;; --- Day 4: Repose Record ---

;; input
;; [1518-11-11 00:04] Guard #2179 begins shift
;; [1518-09-15 00:38] wakes up
;; [1518-10-19 00:22] wakes up
;; [1518-08-14 00:45] falls asleep
;; [1518-10-16 00:47] falls asleep
;; [1518-10-27 00:02] Guard #3181 begins shift

;; (s/def ::guard-id int?)
;; (s/def ::minutes (s/coll-of int?))
;; (s/def ::event (s/keys :req [::guard-id ::year ::month ::day ::hour ::minute ::activity]))
;; (s/def ::coll-event (s/coll-of ::event))
;; (s/def ::guard-minute (s/keys :req [::guard-id ::minutes ::count]))
;; (s/def ::coll-guard-minute (s/coll-of ::guard-minute))
;; (s/explain)
;; (s/explain-data (if (s/valid?)))

(s/fdef read-file
  :args (s/cat :year int? :day int?)
  :ret (s/coll-of string?))
;; (stest/instrument `read-file)
;; (stest/check `read-file)

(defn read-file [year day]
  (->
   (format "resources/%d/input_%02d.txt" year day)
   slurp
   string/split-lines))

(s/fdef parse-int
  :args (s/cat :guard-id (s/nilable string?))
  :ret (s/nilable int?))

(defn parse-int
  "문자열을 숫자로 변환"
  {:test (do #(assert (= (parse-int nil) nil))
             #(assert (= (parse-int "1") 1))
             #(assert (= (parse-int "23") 23)))}
  [guard-id]
  (when guard-id
    (Integer/parseInt guard-id)))


(s/fdef input->event
  :args (s/cat :input string?)
  :ret ::event)

(defn input->event
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

;; (s/def ::guard-data
;;   (s/keys :req [::data ::guard ::sleep-event]))

;; (s/fdef generate-guard-sleep-event
;;   :args (s/alt :event ::event
;;                :guard-data ::guard-data))

(defn generate-guard-sleep-event [{data :data guard :guard sleep-event :sleep-event}
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
       (reduce generate-guard-sleep-event {})
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
   #(assert (= (get-most-minute '(1 2 3 2 3 4 3))
               3))}
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



(defn get-guard-id-minute
  {:test
   (do #(assert (= (get-guard-id-minute {:guard-id 2 :minutes [4 5]})
                   '([2 4] [2 5])))
       #(assert (= (get-guard-id-minute {:guard-id 1 :minutes [2 3 4]})
                   '([1 2] [1 3] [1 4]))))}
  [{guard-id :guard-id
    minutes :minutes}]
  (map (fn [minute] [guard-id minute]) minutes))

(defn part-2 []
  (let [data (read-input)
        guard-id-minute (mapcat get-guard-id-minute data)
        most-same-minute-count (->> guard-id-minute
                                    frequencies
                                    (sort-by val)
                                    reverse
                                    first
                                    second)
        [guard-id minute] (->> guard-id-minute
                               frequencies
                               (filter (fn [[_ v]] (= v most-same-minute-count)))
                               first
                               first)]
    (* guard-id minute)))

(comment
  (part-1)
  (part-2)

  (do
    (test #'parse-int)
    (test #'input->event)
    (test #'sleep-range)
    (test #'get-most-minute)
    (test #'get-most-sleep)
    (test #'get-guard-id-minute))

  '())


;; (require '[clojure.spec.test.alpha :as stest])
;; (get-guard-id-minute {:guard-id 1 :minutes [2 3 4]})
;; (input->event "[1518-11-11 00:04] Guard #2179 begins shift")
;; (input->event "[1518-09-15 00:38] wakes up")
;; (input->event "[1518-08-14 00:45] falls asleep")
;; (stest/instrument)
;; guard-sleep-event
;; [
;;  {:guard-id 123, :sleep-event [[event event] [event event] [event event]]}
;;  {:guard-id 456, :sleep-event [[event event] [event event] [event event]]} 
;;]
;; (s/def ::sleep-event (s/coll-of (s/coll-of ::event)))
;; (s/def ::guard-sleep-event
;;   (s/keys :req [::guard-id ::sleep-event]))
  ;; (->> data
  ;;      (mapcat get-guard-id-minute)
  ;;      frequencies
  ;;      (sort-by vals)
  ;;      reverse
  ;;      first
  ;;      second
  ;;     ;;  first
  ;;     ;;  first
  ;;      )

;; (->> (read-input)
;;      (mapcat get-guard-id-minute)
;;      frequencies
;;      (filter (fn [[_ v]] (= v 17)))
;;      first)

;; (get-most-same-minute (read-input))
;; (def guard-id-minute (mapcat get-guard-id-minute (read-input)))
;; (->> guard-id-minute
;;      frequencies
;;      (sort-by val)
;;      reverse
;;      first
;;      second)
;; (def data (read-input))
;; (get-most-same-minute data)

;; (->> (read-file 2018 4)
;;      sort
;;      (map input->event)
;;      (reduce generate-guard-sleep-event {})
;;      :data
;;      (map sleep-range)
;;      (sort-by :count)
;;      reverse
;;      first
;;      :count)

;; (def data (read-input))
;; (:minutes (first (take 1 data)))


;; (sleep-range [631 [[{:year 1518, :month 2, :day 11, :hour 0, :minute 3, :guard-id nil, :activity :asleep} {:year 1518, :month 2, :day 11, :hour 0, :minute 19, :guard-id nil, :activity :up}]]])

;; (def datas
;;   [{:guard-id 123, :sleep-event [["event" "event"]]}
;;    {:guard-id 456, :sleep-event [["event" "event"]]}])

;; (filter #(= 123 (:guard-id %)) datas)
;; (first
;;  (filter (fn [data] (= 1234 (:guard-id data))) datas))
