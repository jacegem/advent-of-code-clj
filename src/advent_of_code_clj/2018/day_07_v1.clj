(ns advent-of-code-clj.2018.day-07-v1
  (:require [clojure.string :as string]
            [clojure.set :refer [union]]))

(defn read-file [year day & {:keys [type] :as opts}]
  (let [filepath (if (= type :sample)
                   (format "resources/%d/input_%02d_sample.txt" year day)
                   (format "resources/%d/input_%02d.txt" year day))]
    (-> (slurp filepath)
        string/split-lines)))

(defn input->instructions [input]
  (map (fn [line]
         (->> (re-seq #"\b[A-Z]\b" line)
              (map keyword)))
       input))

(defn instructions->steps [instructions]
  (let [prevs (map first instructions)
        nexts (map second instructions)]
    (union (set prevs) (set nexts))))

(let [instructions (-> (read-file 2018 7 :type :sample)
                       input->instructions)
      steps (instructions->steps instructions)]
  steps)

