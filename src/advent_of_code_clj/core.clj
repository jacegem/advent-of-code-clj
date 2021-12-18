(ns advent-of-code-clj.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-input [year day]
  (-> (format "%d/input_%02d.txt" year day)
      (io/resource)
      (slurp)
      (str/trim-newline)))


(get-input 2021 1)
