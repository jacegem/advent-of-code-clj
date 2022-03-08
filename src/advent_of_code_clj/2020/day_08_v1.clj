(ns advent-of-code-clj.2020.day-08-v2
  (:require [clojure.string :as string]
            [tupelo.core :as t]))


(def state {:registers {:acc 0}
            :pc 0
            :programs {0 {:op :acc :value 12}}})

(defn read-file [year day & {:keys [type]}]
  (let [filepath (if (= type :sample)
                   (format "resources/%d/input_%02d_sample.txt" year day)
                   (format "resources/%d/input_%02d.txt" year day))]
    (-> (slurp filepath)
        string/split-lines)))


(defn parse-instructions [inputs]
  (->> (map-indexed (fn [idx line]
                      (let [[_ op value] (re-matches #"(\w+) ([+-]\d+)" line)]
                        {idx {:op (keyword op) :value (Integer/parseInt value)}}))
                    inputs)
       (into {})))

(defn init-state []
  {:acc 0
   :index 0

   :visited-indexes []
   :instructions (-> (read-file 2020 8)
                     parse-instructions)})


(defn run-instruction
  "각 op 에 따라 값을 업데이트"
  [{:keys [acc op value index] :as instruction}]
  (case op
    :nop (assoc instruction :index (inc index))
    :acc (-> instruction
             (assoc :acc (+ acc value))
             (assoc :index (inc index)))
    :jmp (assoc instruction :index (+ index value))))

(defn visit-instructions [{:keys [instructions index visited-indexes acc] :as state}]
  (cond
    (= (count instructions) index) (assoc state :terminated? true)
    (.contains visited-indexes index) (assoc state :loop? true)
    :else (let [visited-indexes (conj visited-indexes index)
                {:keys [op value]} (get instructions index)
                {:keys [acc index]} (run-instruction {:acc acc :op op :value value :index index})]
            (-> state
                (assoc :index index)
                (assoc :acc acc)
                (assoc :visited-indexes visited-indexes)))))


(defn fix-instructions [[id instruction] state]
  (case (:op instruction)
    :nop (assoc-in state [:instructions id :op] :jmp)
    :jmp (assoc-in state [:instructions id :op] :nop)
    nil))


(defn fix-and-visit-instructions [initial-state target-instruction]
  (if-let [updated-state (fix-instructions target-instruction initial-state)]
    (let [result (->> (iterate visit-instructions updated-state)
                      (filter #(or (:terminated? %) (:loop? %)))
                      first)]
      (if (:terminated? result)
        (reduced result)
        initial-state))
    initial-state))


(defn part-2 []
  (let [{:keys [instructions] :as state} (init-state)]
    (-> (reduce fix-and-visit-instructions state instructions)
        :acc)))







;; https://antoniogarrote.wordpress.com/2010/09/15/finite-state-machines-in-clojure-with-jobim/
;; (defprotocol FSM
;;   (init [this initial-message]
;;     "Returns the initial state of the FSM")
;;   (next-transition [this current-state state-data message]
;;     "Defines which transition will be applied provided the current state and the incoming message")
;;   (handle-info [this current-state state-data message]
;;     "Handles messages distinct to events")
;;   (terminate [this state-name state-data]
;;     "Clean up code when the FSM is going to be terminated externally"))

