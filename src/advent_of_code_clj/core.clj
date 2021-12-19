(ns advent-of-code-clj.core
  (:require [clojure.java.io :as io]
            [clojure.string :refer [trim-newline split-lines]]))

(defn get-input [year day]
  (-> (format "%d/input_%02d.txt" year day)
      (io/resource)
      (slurp)
      (trim-newline)))

(defn get-lines [year day]
  (let [input (get-input year day)]
    (split-lines input)))


(get-input 2021 1)
(get-lines 2021 1)
