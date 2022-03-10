(ns advent-of-code-clj.2018.day-07
  (:require [clojure.string :as string]
            [clojure.set :refer [difference union]]))

;; --- Day 7: The Sum of Its Parts ---
;; https://adventofcode.com/2018/day/7


(defn read-file [year day & {:keys [type] :as opts}]
  (let [filepath (if (= type :sample)
                   (format "resources/%d/input_%02d_sample.txt" year day)
                   (format "resources/%d/input_%02d.txt" year day))]
    (-> (slurp filepath)
        string/split-lines)))


(defn instructions->steps [instructions]
  (let [prevs (map first instructions)
        nexts (map second instructions)]
    (union (set prevs) (set nexts))))

(defn req-step [step instructions]
  (->>
   (filter #(= (second %) step) instructions)
   (map #(first %))))

;; (require-steps "E" instructions)

;; (map (fn [step]
;;        {:prevs (require-steps step instructions) :next step})
;;      (steps instructions))

;; (defn prerequisite-steps [steps])

(defn done? [done-steps reqs]
  (every? #((set done-steps) %) reqs))

(defn find-next-step [done-steps remain-steps]
  (->> (filter #(done? done-steps (:req %)) remain-steps)
       (map :step)
       sort  ;; 알파벳 순으로 정렬
       first ;; 첫번째 값
       vector ;; into 사용을 위해 vector로 변환       
       ))

(defn step-reducer [[done-steps remain-steps] _]
  (if (empty? remain-steps)
    (reduced (apply str done-steps))
    (let [next-step (find-next-step done-steps remain-steps)
          done-steps (into done-steps next-step)
          remain-steps (remove #((set next-step) (:step %)) remain-steps)]
      [done-steps remain-steps])))

(defn part-1 []
  (let [instructions (->> (read-file 2018 7 :type :large)
                          (map #(re-seq #"\b[A-Z]\b" %)))
        steps (instructions->steps instructions)
        req-steps (map (fn [step]
                         {:req (req-step step instructions)
                          :step step})
                       steps)]
    (reduce step-reducer [[] req-steps] (range))))

(defn step-time [step]
  (-> (first step)
      int
      (- 4)))

(def ordered-steps (part-1))
(first ordered-steps)


(defn find-idle-worker [workers]
  (->> (filter nil? workers)
       first))

(defn complete-work
  "
    {:step \"A\" :start-time 0 :complete-time 61}
  "
  [time workers]
  (remove #(= time (:complete-time %)) workers))

(let [steps "ABCDE"]
  (map #(nth steps %) (range 0 2)))


(defn assign-work [time step]
  {:start-time time :complete-time  (+ time (step-time step)) :step step})

(defn assign-works
  "Output: [workers remain-step]"
  [worker-count workers remain-step time]
  (let [worker-length (count workers)
        idle-worker-count (- worker-count worker-length)
        remain-count (min idle-worker-count (count (seq remain-step)))
        new-workers (map (fn [idx] (assign-work time (str (nth remain-step idx))))
                         (range remain-count))]
    [(into workers new-workers) (subs remain-step idle-worker-count)]))


(assign-works 5 [] "FFFFF" 0)
(str (nth "ABCDE" 1))
(subs "ABCDE" 3)


(defn worker-reducer [{:keys [worker-count workers remain-step]} time]
  (let [workers (complete-work time workers)
        [workers remain-step] (assign-works worker-count workers remain-step time)]
    (if (and (empty? workers)
             (empty? remain-step))
      (reduced time)
      {:worker-count worker-count :workers workers :remain-step remain-step})))


(reduce worker-reducer {:worker-count 5
                        :workers []
                        :remain-step "ABCDEFGH"} (range 64))

(comment
  (part-1)

  '())


(step-time "A")
(int (first "A"))

(def remain '({:req ("B" "D" "F"), :step "E"}
              {:req (), :step "C"}
              {:req ("C"), :step "F"}
              {:req ("A"), :step "B"}
              {:req ("C"), :step "A"}
              {:req ("A"), :step "D"}))

(find-next-step [] remain)
(find-next-step ["A"] remain)
(into [] (vector "B"))





(step-reducer [[] '({:req ("B" "D" "F"), :step "E"}
                    {:req (), :step "C"}
                    {:req ("C"), :step "F"}
                    {:req ("A"), :step "B"}
                    {:req ("C"), :step "A"}
                    {:req ("A"), :step "D"})] 1)

(step-reducer [["C"]
               '({:req ("B" "D" "F"), :step "E"}
                 {:req ("C"), :step "F"}
                 {:req ("A"), :step "B"}
                 {:req ("C"), :step "A"}
                 {:req ("A"), :step "D"})] 2)

(step-reducer [["C" "A" "F"]
               '({:req ("B" "D" "F"), :step "E"}
                 {:req ("A"), :step "B"}
                 {:req ("A"), :step "D"})] 3)

(find-next-step [] '({:req ("B" "D" "F"), :step "E"}
                     {:req (), :step "C"}
                     {:req ("C"), :step "F"}
                     {:req ("A"), :step "B"}
                     {:req ("C"), :step "A"}
                     {:req ("A"), :step "D"}))

(reduce step-reducer [[] req-steps] (range))





(apply str ["C" "A" "B" "D" "F" "E" nil nil nil nil])
(())
(into [] '("C"))
(into ["A"] '("C" "B"))

(defn conj-reducer [acc val]
  (conj acc val))

(reduce conj-reducer [] [:a :b :c :d])

;; (defn require-steps [steps]
;;   (let [prevs (map first steps)
;;         nexts (map second steps)]
;;     (->>
;;      (union (set prevs) (set nexts))
;;      (map))))

;; (require-steps instructions)


;; (union #{1 2} #{2 3})

;; (defn find-first-step [steps]
;;   (let [prevs (map first steps)
;;         nexts (map second steps)]
;;     (-> (difference (set prevs) (set nexts))
;;         first)))

;; (find-first-step instructions)
;; (difference #{1 2} #{2 3})


;; (->>
;;  (read-file 2018 7 :type :sample)
;;  (map #(re-seq #"\b[A-Z]\b" %)))


;; ;;
;; (def instructions '(("C" "A") ("C" "F") ("A" "B") ("A" "D") ("B" "E") ("D" "E") ("F" "E")))
;; instructions
;; (map first instructions)
;; (map second instructions)
;; (def req-steps
;;   (let [instructions (->>
;;                       (read-file 2018 7 :type :sample)
;;                       (map #(re-seq #"\b[A-Z]\b" %)))
;;         steps (steps instructions)
;;         require-steps (map (fn [step]
;;                              {:req (require-steps step instructions)
;;                               :step step})
;;                            steps)]
;;     require-steps))