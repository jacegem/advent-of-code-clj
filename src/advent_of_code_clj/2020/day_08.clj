(ns advent-of-code-clj.2020.day-08
  (:require [clojure.string :as string]))

;; --- Day 8: Handheld Halting ---
;; https://adventofcode.com/2020/day/8


(defn read-file [year day & {:keys [type] :as opts}]
  (let [filepath (if (= type :sample)
                   (format "resources/%d/input_%02d_sample.txt" year day)
                   (format "resources/%d/input_%02d.txt" year day))]
    (-> (slurp filepath)
        string/split-lines)))

(defn parse-instructions []
  (->> (map-indexed (fn [idx line]
                      (let [[_ op value] (re-matches #"(\w+) ([+-]\d+)" line)]
                        {idx {:id idx :op (keyword op) :value (Integer/parseInt value)}}))
                    (read-file 2020 8))
       (into {})))

(defn run-instruction
  " 각 op 에 따라 값을 업데이트 한다. "
  [{:keys [acc op value index] :as param}]
  (case op
    :nop (assoc param :index (inc index))
    :acc (-> param
             (assoc :acc (+ acc value))
             (assoc :index (inc index)))
    :jmp (assoc param :index (+ index value))))

(defn visit
  "이전에 방문한 op 인지 확인"
  [{:keys [index instructions visited-indexes acc] :as visit-state}]
  (if (.contains visited-indexes index)
    {:acc acc, :loop? true}
    (let [visited-indexes (conj visited-indexes index)
          {:keys [op value]} (get instructions index)
          {:keys [acc index]} (run-instruction {:acc acc :op op :value value :index index})]
      (-> visit-state
          (assoc :index index)
          (assoc :acc acc)
          (assoc :visited-indexes visited-indexes)))))

(defn part-1 []
  (let [instructions (parse-instructions)]
    (->> (iterate visit {:index 0
                         :instructions instructions
                         :visited-indexes []
                         :acc 0
                         :loop? false})
         (filter #(= true (:loop? %)))
         first)))

(defn visit-until-last [{:keys [index instructions visited-indexes acc] :as visit-state}]
  (cond
    (= (count instructions) index) (assoc visit-state :terminated? true)
    (.contains visited-indexes index) (assoc visit-state :loop? true)
    :else (let [visited-indexes (conj visited-indexes index)
                {:keys [op value]} (get instructions index)
                {:keys [acc index]} (run-instruction {:acc acc :op op :value value :index index})]
            (-> visit-state
                (assoc :index index)
                (assoc :acc acc)
                (assoc :visited-indexes visited-indexes)))))


(defn change-instruction [{:keys [id op]} instructions]
  (case op
    :nop (assoc-in instructions [id :op] :jmp)
    :jmp (assoc-in instructions [id :op] :nop)
    nil))


(defn find-normally-terminated-acc [{:keys [initial-instructions] :as initial-state}
                                    [_ target-instruction]]
  (if-let [updated-instructions (change-instruction  target-instruction initial-instructions)]
    (let [result (->> (iterate visit-until-last {:index 0
                                                 :instructions updated-instructions
                                                 :visited-indexes []
                                                 :acc 0})
                      (filter #(or (:loop? %) (:terminated? %)))
                      first)]
      (if (:terminated? result)
        (reduced (assoc initial-state :result result))
        initial-state))
    initial-state))

(defn fix [instructions]
  (reduce find-normally-terminated-acc {:initial-instructions instructions}
          instructions))

(defn part-2 []
  (let [instructions (parse-instructions)]
    (fix instructions)))


(comment
  (part-1)
  (part-2)
  '())



;; (def instructions (parse-instructions))
;; (def ins (change-instruction (nth instructions 7) instructions))
;; (reduce find-normally-terminated-acc instructions instructions)
;; (def instructions
;;   (parse-instructions))
;; instructions
;; (defn visit2 [{:keys [index instructions visited-indexes acc] :as visit-state}]
;;   (if (.contains visited-indexes index)
;;     :A
;;     (let [visited-indexes (conj visited-indexes index)]
;;       visited-indexes)))
;; (.contains [] 0)
;; (get instructions 0)
;; (visit2 {:index 0
;;          :instructions instructions
;;          :visited-indexes []
;;          :acc 0
;;          :loop? false})
;; (map-indexed hash-map '({:op :acc, :value 7}
;;                         {:op :jmp, :value 492}))

;; (map-indexed (fn [idx item] {idx (assoc item :id idx)})
;;              '({:op :acc, :value 7}
;;                {:op :jmp, :value 492}))

