(ns advent-of-code-clj.2018.day-02 
  (:require [clojure.string :as string]
            [clojure.data :refer [diff]]
            ))


;; https://adventofcode.com/2018/day/2
;; --- Day 2: Inventory Management System ---

(def line "pnebjusalrdgckzfihvtxywyml")
(def words
  (string/split line #""))

(def input
    (slurp "resources/2018/input_02.txt"))

(def splited
    (string/split-lines input))



(def parsed
    (map #(string/split % #"") splited))

parsed

(defn has-times [coll n]
  (if (some #{n}
            (vals
             (frequencies coll)))
    true
    false))
    
(defn get-count [times]
  (count
   (filter #(= % true)
           (map #(has-times % times) parsed))))

(def part-1
  (* (get-count 2) (get-count 3)))
  
part-1


(first parsed)
(second parsed)

(count (filter some?
               (first
                (diff (first parsed) (second parsed)))))


(loop [[v & rem] '(1 2 3 4 5)]
  (if (seq? v)
    (recur rem)
    "end")
  )

(loop [[box & rem] parsed]
  (if-let [common (get-common? box rem)]
    common
    
    )

  )




(comment
  (contains?
   (vec
    (vals
     (frequencies words))) 3)

  (contains? [1 2 3 4] 5)

  (defn has-times [coll n]
    (contains?
     (vec
      (vals
       (frequencies coll))) n))


  (some #{2}
        (vals
         (frequencies (first parsed))))


  (has-times (first parsed) 3)

  (first parsed)

  (has-times (first parsed) 3)

  (def count-2
    (get-count 2))
  (def count-3
    (get-count 3))

  count-2
  count-3

  (* count-2 count-3))

         '()