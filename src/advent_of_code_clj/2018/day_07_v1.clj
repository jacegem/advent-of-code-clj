(ns advent-of-code-clj.2018.day-07-v1
  (:require [clojure.string :as string]
            [clojure.set :refer [union]]))

;; --- Day 7: The Sum of Its Parts ---
;; https://adventofcode.com/2018/day/7

(defn read-file [year day & {:keys [type] :as opts}]
  (let [filepath (if (= type :sample)
                   (format "resources/%d/input_%02d_sample.txt" year day)
                   (format "resources/%d/input_%02d.txt" year day))]
    (-> (slurp filepath)
        string/split-lines)))

(defn extract-instructions
  "입력값에서 instruction 추출"
  [input]
  (map (fn [line]
         (->> (re-seq #"\b[A-Z]\b" line)
              (map keyword)))
       input))

(defn find-steps [instructions]
  (let [prevs (map first instructions)
        nexts (map second instructions)]
    (union (set prevs) (set nexts))))

(defn find-prev-req-steps
  "사전에 필요한 steps 확인"
  {:test #(do (assert (= (find-prev-req-steps :E
                                              '((:C :A) (:C :F) (:A :B) (:A :D) (:B :E) (:D :E) (:F :E)))
                         '(:B :D :F))))}
  [step instructions]
  (->> (filter #(= (second %) step) instructions)
       (map #(first %))))
;; (test #'find-prev-require-steps) 

(defn create-steps-with-prev-reqs
  {:test
   #(do (assert (= (create-steps-with-prev-reqs [:C :A :B :D :F :E]
                                                '((:C :A) (:C :F) (:A :B) (:A :D) (:B :E) (:D :E) (:F :E)))
                   '({:prev-req-steps (), :step :C}
                     {:prev-req-steps (:C), :step :A}
                     {:prev-req-steps (:A), :step :B}
                     {:prev-req-steps (:A), :step :D}
                     {:prev-req-steps (:C), :step :F}
                     {:prev-req-steps (:B :D :F), :step :E}))))}
  [steps instructions]
  (->> (map (fn [step]
              {:prev-req-steps (find-prev-req-steps step instructions)
               :step step})
            steps)))
;; (test #'create-steps-with-prev-reqs)

(defn startable-step?
  {:test #(do (assert (= (startable-step? [:C] {:prev-req-steps '(:C), :step :A})
                         true))
              (assert (= (startable-step? [:C] {:prev-req-steps '(:A), :step :D})
                         false)))}
  [done-steps step-with-prev-reqs]
  (every? #((set done-steps) %) (:prev-req-steps step-with-prev-reqs)))
;; (test #'done-available-step?)


(defn find-next-step [done-steps remain-steps]
  (->> (filter #(startable-step? done-steps %) remain-steps)
       (map :step)
       sort  ;; 알파벳 순으로 정렬
       first ;; 첫번째 값
       ))

(defn exec-instructions
  "다음 단계을 찾아서 실행"
  [{:keys [done-steps remain-steps]}]
  (let [next-step (find-next-step done-steps remain-steps)
        updated-done-steps (conj done-steps next-step)
        updated-remain-steps (remove #(= (:step %) next-step) remain-steps)]
    {:done-steps updated-done-steps
     :remain-steps updated-remain-steps}))

(defn part-1 []
  (let [instructions (-> (read-file 2018 7)
                         extract-instructions)
        steps (find-steps instructions)
        steps-with-prev-reqs (create-steps-with-prev-reqs steps instructions)]
    (->> (iterate exec-instructions {:done-steps []
                                     :remain-steps steps-with-prev-reqs})
         (drop-while #(-> (:remain-steps %)
                          seq))
         first
         :done-steps
         (map name)
         (apply str))))

(part-1)

(defn create-workers [n]
  (reduce (fn [workers worker-id]
            (assoc workers worker-id {:worker-id worker-id
                                      :step nil
                                      :complete-time nil})) {} (range n)))


(defn calc-complete-time
  {:test #(do (assert (= (calc-complete-time 1 :A)
                         62)))}
  [current-time step]
  (-> (name step)
      first
      int
      (- 4)
      (+ current-time)
      ))
;; (test #'calc-complete-time)
(calc-complete-time 0 :A)

(defn assign-to-workers [{:keys [done-steps remain-steps current-time workers]} worker-id]
  (if (empty? remain-steps)
    (reduced
     {:done-steps done-steps
      :remain-steps remain-steps
      :current-time current-time
      :workers workers})
    (let [next-step (find-next-step done-steps remain-steps)
          updated-done-steps (conj done-steps next-step)
          updated-remain-steps (remove #(= (:step %) next-step) remain-steps)
          updated-workers (-> workers
                              (assoc-in [worker-id :step] next-step)
                              (assoc-in [worker-id :complete-time] (calc-complete-time current-time next-step)))]
      {:done-steps updated-done-steps
       :remain-steps updated-remain-steps
       :current-time current-time
       :workers updated-workers})))


(def remain-steps '({:prev-req-steps (), :step :C}
                    {:prev-req-steps (:C), :step :A}
                    {:prev-req-steps (:A), :step :B}
                    {:prev-req-steps (:A), :step :D}
                    {:prev-req-steps (:C), :step :F}
                    {:prev-req-steps (:B :D :F), :step :E}))



(defn find-complete-steps
  {:test
   #(do (assert (= (find-complete-steps 12 {0 {:worker-id 0, :step :C, :complete-time 12}})
                   '(:C))))}
  [current-time workers]
  (->> workers
       (filter (fn [[_ worker]] (= (:complete-time worker) current-time)))
       vals
       (map :step)))
;; (test #'find-complete-steps)
;; (find-complete-steps 12 {0 {:worker-id 0, :step :C, :complete-time 12}})

;; 
(defn update-workers
  "완료된 목록을 찾아서, workers 를 업데이트"
  [completed-steps workers]
  (->>
   (map (fn [[worker-id worker]]
          (if (.contains completed-steps (:step worker))
            {worker-id
             (-> worker
                 (assoc-in [:complete-time] nil)
                 (assoc-in [:step] nil))}
            {worker-id worker}))
        workers)
   (into {})))

(update-workers '(:C) {0 {:worker-id 0, :step :C, :complete-time 12}
                       1 {:worker-id 1, :step :D, :complete-time 12}})
;; (.contains '(:C) :C)

;; (def updated-workers '({0 {:worker-id 0, :step nil, :complete-time nil}} {1 {:worker-id 1, :step :D, :complete-time 12}}))
;; updated-workers
;; (->> updated-workers
;;      (into {}))

(defn exec-instructions-with-workers
  [{:keys [done-steps remain-steps workers current-time]}]
  (let [completed-steps (find-complete-steps current-time workers)
        updated-done-steps (into done-steps completed-steps)
        updated-workers (update-workers completed-steps workers)
        idle-ids (->> (filter (fn [[_ worker]] (nil? (:step worker))) workers)
                      keys)]
    (-> (reduce assign-to-workers {:done-steps updated-done-steps
                                   :remain-steps remain-steps
                                   :current-time current-time
                                   :workers updated-workers}
                idle-ids)
        (assoc :current-time (inc current-time)))))

(exec-instructions-with-workers {:done-steps []
                                 :remain-steps remain-steps
                                 :workers (create-workers 5)
                                 :current-time 0})



(exec-instructions-with-workers {:done-steps [:C :A :B :D :F :A :B :E],
                                 :remain-steps (),
                                 :current-time 63,
                                 :workers
                                 {0 {:worker-id 0, :step :C, :complete-time 63},
                                  1 {:worker-id 1, :step :E, :complete-time 127},
                                  2 {:worker-id 2, :step nil, :complete-time nil},
                                  3 {:worker-id 3, :step :D, :complete-time 64},
                                  4 {:worker-id 4, :step :F, :complete-time 66}}})


(->
 (reduce assign-to-workers {:done-steps []
                            :remain-steps remain-steps
                            :current-time 0
                            :workers  (create-workers 5)}
         '(0 1 2 3 4))
 (assoc :current-time (inc 0)))



(->> (exec-instructions-with-workers {:done-steps []
                                      :remain-steps remain-steps
                                      :workers (create-workers 5)
                                      :current-time 0})
     (take 1))

(->> (iterate exec-instructions-with-workers {:done-steps []
                                              :remain-steps remain-steps
                                              :workers (create-workers 5)
                                              :current-time 0})

     (take 129)
     last)

(def steps (find-steps (-> (read-file 2018 7 :type :sampel)
                           extract-instructions)))

(defn all-workers-are-idle? [{:keys [workers remain-steps]}]  
  (->> workers
       (every? (fn [[_ worker]] (nil? (:step worker))))))

(all-workers-are-idle? {:done-steps [:C :A :B :D :F :A :B :E :C :D :F :E],
                         :remain-steps (),
                         :current-time 128,
                         :workers
                         {0 {:worker-id 0, :step nil, :complete-time nil},
                          1 {:worker-id 1, :step nil, :complete-time nil},
                          2 {:worker-id 2, :step nil, :complete-time nil},
                          3 {:worker-id 3, :step nil, :complete-time nil},
                          4 {:worker-id 4, :step nil, :complete-time nil}}})

(exec-instructions-with-workers {:done-steps []
                                              :remain-steps remain-steps
                                              :workers (create-workers 5)
                                              :current-time 0})

(empty? '())

(->> (iterate exec-instructions-with-workers {:done-steps []
                                              :remain-steps remain-steps
                                              :workers (create-workers 5)
                                              :current-time 0})
    ;; 모든 worker step 이 nil 일 때까지
    ;;  
     (filter #(empty? (:remain-steps %)))
     (filter all-workers-are-idle?)
     first)


    ;;  (drop-while #(-> (:remain-steps %)
    ;;                   seq))
    ;;  first)



(defn part-2 []
  (let [instructions (-> (read-file 2018 7)
                         extract-instructions)
        steps (find-steps instructions)
        steps-with-prev-reqs (create-steps-with-prev-reqs steps instructions)]
    
    (->> (iterate exec-instructions-with-workers {:done-steps []
                                                  :remain-steps steps-with-prev-reqs
                                                  :workers (create-workers 5)
                                                  :current-time 0})
         (filter #(empty? (:remain-steps %)))
         (filter all-workers-are-idle?)
         first
         )))


(part-2)



(comment
  (part-1)
  '())

(->>
 (iterate exec-instructions {:done-steps [] :remain-steps remain-steps})
 (take 7)
 last
 (:remain-steps))

(def remain-steps '({:prev-req-steps (:C), :step :A}
                    {:prev-req-steps (:C), :step :F}
                    {:prev-req-steps (:A), :step :D}
                    {:prev-req-steps (:A), :step :B}
                    {:prev-req-steps (), :step :C}
                    {:prev-req-steps (:B :D :F), :step :E}))

(->> (iterate inc 5)
     (take-while #(< % 10)))

(->> (range 50)
     (take-while #(< % 10)))
;; (filter #(= (:id %) 1) '({:id 2} {:id 1}))



;; (assoc-in {} [1 2] 3)

;; (-> workers
;;     (assoc-in [1 :step] :A)
;;     (assoc-in [1 :complete-time] 62))


;; (def workers (create-workers 5))
;; (assign-to-workers {:done-steps []
;;                     :remain-steps remain-steps
;;                     :current-time 0
;;                     :workers (create-workers 5)} 1)

;; (empty? '())

;; (assign-to-workers {:done-steps [:C :A :B :D :F :A :B :E :C]
;;                     :remain-steps ()
;;                     :current-time 63
;;                     :workers {0 {:worker-id 0, :step nil, :complete-time nil},
;;                               1 {:worker-id 1, :step :E, :complete-time 127},
;;                               2 {:worker-id 2, :step nil, :complete-time nil},
;;                               3 {:worker-id 3, :step :D, :complete-time 64},
;;                               4 {:worker-id 4, :step :F, :complete-time 66}}}
;;                    '(0 2))


;; (defn complete-steps
;;   "complete-time 을 확인하여 step 을 완료"
;;   [current-time workers]
;;   (->>
;;    (map (fn [[worker-id {:keys [complete-time] :as worker}]]
;;           (if (= complete-time current-time)
;;             (-> worker
;;                 (assoc :step nil))))

;;         workers)
;;    (into {})))


;; (mapv (fn [[k v]] {k v}) {1 :A 2 :B})

;; (->>
;;  (map (fn [[k v]] [k v]) {1 :A 2 :B})
;;  (into {}))
;; (hash-map {1 :A} {2 :B})