(ns advent-of-code-clj.2018.day-07-v2
  (:require [clojure.set :refer [union]]
            [clojure.string :as string]))

(defn read-file [year day & {:keys [type] :as opts}]
  (let [filepath (if (= type :sample)
                   (format "resources/%d/input_%02d_sample.txt" year day)
                   (format "resources/%d/input_%02d.txt" year day))]
    (-> (slurp filepath)
        string/split-lines)))

(defn str->instructions
  "입력값에서 instruction 추출"
  {:test #(do (assert (= (str->instructions '("Step V must be finished before step H can begin."))
                         '((:V :H)))))}
  [inputs]
  (map #(->> (re-seq #"\b[A-Z]\b" %)
             (map keyword))
       inputs))
;; (test #'str->instructions)

(defn find-steps
  "모든 step 목록 확인"
  {:test #(do (assert (= (find-steps '((:A :B) (:B :C)))
                         #{:A :B :C})))}
  [instructions]
  (let [prevs (map first instructions)
        nexts (map second instructions)]
    (union (set prevs) (set nexts))))
;; (test #'find-steps)

(defn find-prev-req-steps
  "사전에 필요한 steps 확인"
  {:test
   #(do (assert (= (find-prev-req-steps :E
                                        '((:C :A) (:C :F) (:A :B) (:A :D) (:B :E) (:D :E) (:F :E)))
                   '(:B :D :F))))}
  [step instructions]
  (->> (filter #(= (second %) step) instructions)
       (map #(first %))))
;; (test #'find-prev-req-steps)

(defn create-steps-with-prev-reqs
  "사전에 필요한 steps 데이터 생성"
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


(defn find-completed-work
  "완료된 작업을 찾는다"
  {:test #(do (assert (= (find-completed-work 1 {1 {:id 1 :step :A :complete-time 1}
                                                 2 {:id 2 :step :B :complete-time 1}})
                         '(:A :B))))}
  [current-time workers]
  (->> workers
       (filter (fn [[_ worker]] (= (:complete-time worker) current-time)))
       vals
       (map :step)))
;; (test #'find-completed-work)

(defn update-workers
  "완료된 목록을 찾아서, workers 를 업데이트"
  {:test
   #(do (assert (= (update-workers [:A] {0 {:id 0 :step :A :complete-time 0}
                                         1 {:id 1 :step :B :complete-time 0}})
                   {0 {:id 0, :step nil, :complete-time nil}, 1 {:id 1, :step :B, :complete-time 0}})))}
  [completed-steps workers]
  (->> (map (fn [[worker-id worker]]
              (if (.contains completed-steps (:step worker))
                {worker-id (-> worker
                               (assoc-in [:complete-time] nil)
                               (assoc-in [:step] nil))}
                {worker-id worker}))
            workers)
       (into {})))
;; (test #'update-workers)

(defn update-completed-steps
  {:test
   #(do (assert (= (update-completed-steps 1 '(:A :B) {0 {:worker-id 1, :step :C, :complete-time 1}})
                   {:done-steps '(:C :A :B), :workers {0 {:worker-id 1, :step nil, :complete-time nil}}})))}
  [time done-steps workers]
  (let [completed-steps (find-completed-work time workers)
        updated-workers (update-workers completed-steps workers)
        updated-done-steps (into done-steps completed-steps)]
    {:done-steps updated-done-steps
     :workers updated-workers}))
;; (test #'update-completed-steps)

(defn startable-step?
  {:test #(do (assert (= (startable-step? [:C] {:prev-req-steps '(:C), :step :A})
                         true))
              (assert (= (startable-step? [:C] {:prev-req-steps '(:A), :step :D})
                         false)))}
  [done-steps remain-step]
  (every? #((set done-steps) %) (:prev-req-steps remain-step)))
;; (test #'startable-step?)

(defn find-startable-steps [done-steps remain-steps]
  (->> (filter #(startable-step? done-steps %) remain-steps)
       (map :step)
       sort  ;; 알파벳 순으로 정렬
       ))

(defn create-workers [n]
  (reduce (fn [workers worker-id]
            (assoc workers worker-id {:worker-id worker-id
                                      :step nil
                                      :complete-time nil})) {} (range n)))

(defn calc-complete-time
  {:test #(do (assert (= (calc-complete-time 1 :A)
                         62)))}
  [time step]
  (-> (name step)
      first
      int
      (- 4)
      ;; (- 60)
      (+ time)))
;; (test #'calc-complete-time)

(defn assign-to-worker
  {:test
   #(do (assert (= (assign-to-worker {:current-time 1
                                      :started-steps []
                                      :workers {0 {:worker-id 0, :step nil, :complete-time nil},
                                                1 {:worker-id 1, :step nil, :complete-time nil}}}
                                     :A)
                   {:current-time 1,
                    :started-steps [:A],
                    :workers {0 {:worker-id 0, :step :A, :complete-time 62},
                              1 {:worker-id 1, :step nil, :complete-time nil}}})))}
  [{:keys [time done-steps remain-steps workers]} next-step]
  (if (some (fn [[_ worker]] (= (:step worker) nil)) workers)
    (let [[worker-id worker] (first (filter (fn [[_ worker]] (= (:step worker) nil)) workers))
          updated-worker (assoc worker :step next-step :complete-time (calc-complete-time time next-step))
          updated-workers (assoc workers worker-id updated-worker)
          remain-steps (remove #(= (:step %) next-step) remain-steps)]

      {:time time
       :workers updated-workers
       :done-steps done-steps
       :remain-steps remain-steps})
    (reduced {:time time
              :workers workers
              :done-steps done-steps
              :remain-steps remain-steps})))


(defn assign-to-workers
  [{:keys [time done-steps remain-steps workers]}]
  (let [time (inc time)
        {:keys [done-steps workers]} (update-completed-steps time done-steps workers)
        startable-steps (find-startable-steps done-steps remain-steps)]
    (reduce assign-to-worker {:time time
                              :done-steps done-steps
                              :remain-steps remain-steps
                              :workers workers}
            startable-steps)))


(defn all-workers-are-idle? [{:keys [workers]}]
  (->> workers
       (every? (fn [[_ worker]] (nil? (:step worker))))))


(defn part-2 [& {:keys [worker-count] :or {worker-count 5} :as opts}]
  (let [instructions (-> (read-file 2018 7)
                         str->instructions)
        steps (find-steps instructions)
        steps-with-prev-reqs (create-steps-with-prev-reqs steps instructions)
        time -1
        done-steps []
        workers (create-workers worker-count)]
    (->> (iterate assign-to-workers {:time time
                                     :done-steps done-steps
                                     :remain-steps steps-with-prev-reqs
                                     :workers workers})
         (filter #(empty? (:remain-steps %)))
         (filter all-workers-are-idle?)
         first)))


(comment
  (part-2)
  '())


;; (def steps
;;   (let [instructions (-> (read-file 2018 7)
;;                          str->instructions)
;;         steps (find-steps instructions)
;;         steps-with-prev-reqs (create-steps-with-prev-reqs steps instructions)]
;;     steps-with-prev-reqs))



;; (assign-to-workers {:current-time 1
;;                     :done-steps []
;;                     :remain-steps steps
;;                     :workers (create-workers 3)})

;; (defn exec-instructions-with-workers
;;   [{:keys [current-time done-steps steps workers]}]
;;   (->>
;;    (iterate assign-to-workers {:current-time (inc current-time)
;;                                :done-steps done-steps
;;                                :steps steps
;;                                :workers workers})
;;    (take 5)))

;; (when-not (= workers updated-workers)
;;   (map (fn [[_ v]] (when (:step v) (print (:step v) (:complete-time v)))) updated-workers))
;; (map (fn [[_ v]] (when (:step v) (print (:step v) (:complete-time v)))) {0 {:worker-id 0, :step :A, :complete-time 62},
                                                                        ;;  1 {:worker-id 1, :step nil, :complete-time nil}})
;; (test #'assign-to-worker)


;; (def steps
;;   (let [instructions (-> (read-file 2018 7)
;;                          str->instructions)
;;         steps (find-steps instructions)
;;         steps-with-prev-reqs (create-steps-with-prev-reqs steps instructions)]
;;     steps-with-prev-reqs))
;; (->> (read-file 2018 7)
;;      first
;;      (re-seq #"\b[A-Z]\b")
;;      (map keyword))





