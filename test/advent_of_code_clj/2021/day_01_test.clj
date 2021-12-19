(ns advent-of-code-clj.2021.day-01-test
  (:require [clojure.test :refer [deftest testing is run-tests]]
            [advent-of-code-clj.2021.day-01 :refer [part-1 part-2]]
            [advent-of-code-clj.core :refer [get-input]]
            [clojure.repl :refer [doc]]))

(deftest part1
  (let [expected 1715]
    (is (= expected (part-1 (get-input 2021 01))))))

(deftest part2
  (let [expected 1739]
    (is (= expected (part-2 (get-input 2021 01))))))
