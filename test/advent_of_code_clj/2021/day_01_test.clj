(ns advent-of-code-clj.2021.day-01-test
  (:require [clojure.test :refer [deftest testing is run-tests]]
            [advent-of-code-clj.2021.day-01 :refer [solve]]
            [advent-of-code-clj.core :refer [get-input]]
            [clojure.repl :refer [doc]]))

(deftest day-01
  (let [expected 1715]
    (is (= expected (solve (get-input 2021 01))))))

(run-tests 'advent-of-code-clj.2021.day-01-test)

;; 기존 이름이 남아 있어서 삭제
(ns-unmap *ns* 'part1)
