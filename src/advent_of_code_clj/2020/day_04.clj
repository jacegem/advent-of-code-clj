(ns advent-of-code-clj.2020.day-04
  (:require [clojure.string :as string]))

;; --- Day 4: Passport Processing ---
;; https://adventofcode.com/2020/day/4

(defn read-file [year day]
  (->
   (format "resources/%d/input_%02d.txt" year day)
   slurp
   string/split-lines))

(defn str->passport
  "문자열을 passport 해쉬맵으로 변환"
  {:test
   #(do (assert (= (str->passport "iyr:2010 ecl:gry hcl:#6b5442")
                   {:iyr "2010", :ecl "gry" :hcl "#6b5442"})))}
  [str]
  (->> (re-seq #"(\w+):(\S+)" str)
       (map (fn [[_ key value]] {(keyword key) value}))
       (apply merge)))
;; (test #'str->passport)

(defn valid-passport?
  " passport 유효성 확인 
    [:byr :iyr :eyr :hgt :hcl :ecl :pid] 모두 있으면 Valid "
  {:test
   #(do (assert (= (valid-passport? {:byr 1 :iyr 2 :eyr 3 :hgt 4 :hcl 5 :ecl 6 :pid 7})
                   true))
        (assert (= (valid-passport? {:byr 1 :iyr 2 :eyr 3 :hgt 4 :hcl 5 :ecl 6})
                   false))
        (assert (= (valid-passport? {:byr 1 :iyr 2 :eyr 3 :hgt 4 :ecl 6 :pid 7})
                   false)))}
  [passport]
  (every? (set (keys passport)) [:byr :iyr :eyr :hgt :hcl :ecl :pid]))
(test #'valid-passport?)
(valid-passport? {:ecl "hzl", :hgt "75in", :cid "233", :pid "269157261", :iyr "2020", :byr "1973", :eyr "2029"})


(defn part-1 []
  (->> (read-file 2020 4)
       (partition-by empty?)
       (map #(string/join " " %))
       (remove empty?)
       (map str->passport)
       (filter valid-passport?)
       count))

(defn parse-int
  {:test
   #(do (assert (= (parse-int nil) nil))
        (assert (= (parse-int "A") nil))
        (assert (= (parse-int "B1") nil))
        (assert (= (parse-int "23") 23))
        (assert (= (parse-int 45) 45)))}
  [s]
  (cond
    (int? s) s
    (string? s) (when-let [str (re-matches #"\d+" s)]
                  (Integer/parseInt str))))

(defn between?
  "min max 의 사이값인지 확인"
  {:test
   #(do (assert (= (between? "1" 0 2) true))
        (assert (= (between? 2 0 2) true))
        (assert (= (between? "3" 0 2) false)))}
  [value min max]
  (let [value (parse-int value)]
    (and (>= value min) (<= value max))))

(defn valid-hgt?
  "valid cm 150 ~ 193
   valid in 59 ~ 76"
  {:test
   #(do (assert (= (valid-hgt? "150cm") true))
        (assert (= (valid-hgt? "76in") true))
        (assert (= (valid-hgt? "140cm") false))
        (assert (= (valid-hgt? "86in") false)))}
  [str]
  (let [[_ value unit] (re-find #"(\d+)(\w+)" str)]
    (case unit
      "cm" (between? value 150 193)
      "in" (between? value 59 76)
      false)))
;; (test #'valid-hgt?)

(defn valid-field?
  {:test
   #(do
      (assert (= (valid-field? [:byr "1920"]) true))
      (assert (= (valid-field? [:iyr "2020"]) true))
      (assert (= (valid-field? [:eyr "2022"]) true))
      (assert (= (valid-field? [:hgt "150cm"]) true))
      (assert (= (valid-field? [:hcl "#602927"]) "#602927"))
      (assert (= (valid-field? [:ecl "hzl"]) "hzl"))
      (assert (= (valid-field? [:pid "123456789"]) "123456789"))
      (assert (= (valid-field? [:cid nil]) true)))}
  [[key value]]
  (case key
    :byr (between? value 1920 2002)
    :iyr (between? value 2010 2020)
    :eyr (between? value 2020 2030)
    :hgt (valid-hgt? value)
    :hcl (re-matches #"#[\da-f]{6}" value)
    :ecl (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} value)
    :pid (re-matches #"\d{9}" value)
    :cid true
    false))
;; (test #'valid-field?)

(defn part-2 []
  (->> (read-file 2020 4)
       (partition-by empty?)
       (map #(string/join " " %))
       (remove empty?)
       (map str->passport)
       (filter valid-passport?)
       (filter #(every? valid-field? %))
       count))

(comment
  (part-1)
  (part-2)

  (test #'str->passport)
  (test #'valid-passport?)
  (test #'parse-int)
  (test #'between?)
  (test #'valid-hgt?)
  (test #'valid-field?)

  '())

;; (def valid-data
;;   {:iyr "2010", :ecl "gry", :hgt "181cm", :pid "591597745", :byr "1920", :hcl "#6b5442", :eyr "2029", :cid "123"})

;; (re-find #"(\d+)(\w+)" "181cm")
;; (re-matches #"(\d+)(\w+)" "x181cm")

;; (re-matches #"^\d{9}$" "5915977459")
;; (every? valid-field? valid-data)
;; (valid-field? [:byr "2003"])
;; ("iyr:2010 ecl:gry hgt:181cm pid:591597745 byr:1920 hcl:#6b5442 eyr:2029 cid:123"
;;    "cid:223 byr:1927 hgt:177cm hcl:#602927 iyr:2016 pid:404183620 ecl:amb eyr:2020"
;;    "byr:1998 ecl:hzl cid:178 hcl:#a97842 iyr:2014 hgt:166cm pid:594143498 eyr:2030")


