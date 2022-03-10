(ns advent-of-code-clj.2018.day-01
  (:require [clojure.string :as string]
            [clojure.repl :refer [doc]]))

;; https://adventofcode.com/2018/day/1
;; --- Day 1: Chronal Calibration ---

(def input
    (slurp "resources/2018/input_01.txt"))

input

(def splited
    (string/split-lines input))

splited

(def parsed
    (map #(Integer/parseInt %) splited))

parsed

(def part-1
    (reduce + 0 parsed))

part-1

(reductions + 0 parsed)

(def sum-list
  (reductions + 0 parsed))

(frequencies sum-list)

(doc frequencies)

(loop [[v & rem] sum-list]
  (do
    (println v)
    (if (nil? v)
      "end"
      (recur rem)))
  )

(loop [result #{}
       [v & rem] sum-list] 
  (if (nil? v)
   "NOT FOUND"  
    (if (result v)
      v
      (recur (conj result v) rem))) 
)


(filter #(= % 241) sum-list)

(take 5 sum-list)

(let [[v & rem] sum-list]
  (println rem))
  

(take 5 sum-list)
(take 5 (frequencies sum-list))


(doc frequencies)
(frequencies '(1 2 3 4 5 6 1 2 3 1 3 3 3 4 7))



(filter (fn [[k v]] (> v 1))
        (frequencies '(2 3 4 5 6 7 4 3 2 2)))

(take 5 sum-list)

(nth sum-list 1)

(filter (fn [[k v]] (> v 1))
        (frequencies [2 3 4 4 3 1 1]))

(loop [result #{0}
       sum 0
       [v & rem] (cycle parsed)]
  (let [next-sum (+ sum v)]
    (if (result next-sum)
      next-sum
      (recur (conj result next-sum) next-sum rem)    
    )
  )
)

;; loop recur, 하이레벨 함수를 이용한 리펙토한
;; doc
;; vscode - calva
;; 함수의 조합으로 
;; loop, recur 금지, 시퀀스 함수 사용
;; rem 이름 문제 발생 가능
;; first, rest 함수 사용



(take 10 (cycle [1 2 3]))


parsed

(for [num parsed]
        (do 
          (println num)
          (println num) 
          )
)

;; found = []
;; acc = 0
;; for value in [1 -2 3 1]:
;;     acc += value
;;     found.append(acc)


;; [1 -2 3 1] => [1 -1 2 3 4 2 5, ...]

(loop [current-value 1 remaining-items [2 3 4 1 2 3] coll #{}]
  (if (coll current-value)
    current-value
    (recur (first remaining-items)
           (rest remaining-items)
           (conj coll current-value))))

(doc reductions)

(reductions + 0 [1 2 3 4])

;; 0
;; + 0 1 => 1
;; + 1 2 => 3
;; + 3 3 => 6
;; + 6 4 => 10



(comment
    ; https://clojuredocs.org/clojure.string/split-lines  
    ; https://clojuredocs.org/clojure.core/int
    ; https://github.com/jasilven/aoc-2018-clojure/blob/master/src/day1.clj
    ; https://github.com/baritonehands/advent-of-code-2018/blob/master/src/aoc/dec1.clj
    (Integer/parseInt "-11")
    (slurp "resources/2018/input_01.txt")  
    (load-file "src/advent_of_code_clj/core.clj")
    (count [0])
    (loop [[parsed sum result] [parsed 0 #{0}]]
      (println parsed)
      (if (seq? parsed)
        (recur [(rest parsed) sum result]))
      )
      (let [sum 0 result #{0}]
      (for [num parsed]
        (do 
          (println num)
          (println num) 
          )
        )
    )
'())
