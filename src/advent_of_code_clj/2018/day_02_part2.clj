(ns advent-of-code-clj.2018.day-02-part2
  (:require [clojure.string :as string]
            [clojure.data :refer [diff]]))


(defn read-file []
  (->
   (slurp "resources/2018/input_02.txt")
   string/split-lines))

(defn diff-str [a b]
  (diff (seq a) (seq b)))

(defn diff-one-char? [a b]
  (->> (diff-str a b)
       first
       (filter some?)
       count
       (= 1)))

(defn find-common [a b]
  (apply str (last (diff-str a b))))

(defn part-2 []
  (let [ids (read-file)]
    (for [a ids
          b ids
          :when  (diff-one-char? a b)]
      (find-common a b))))

(part-2)




(comment
  (defn read-file []
    (->
     (slurp "resources/2018/input_02.txt")
     string/split-lines))
  (seq "abc")

    ;; 하나와 나머지를 비교
  (defn diff-one-char? [a b]
    (->> (diff (seq a) (seq b))
         first
         (filter some?)
         count
         (= 1)))

  (diff-str "abc" "abd")
  (diff-one-char? "pnebjqsalhdgcdzfihvbxywomu" "pnebjqsalhdgcdafihvbxywomu")
  (diff (seq "pnebjqsalhdgcdzfihvbxywomu") (seq "pnebjqsalhdgcdafihvbxywomu"))
  (filter some? [nil nil nil nil nil nil nil nil nil nil nil nil nil nil \z])
  (apply str (last (diff (seq "pnebjqsalhdgcdzfihvbxywomu") (seq "pnebjqsalhdgcdafihvbxywomu"))))
    ;; 순차적으로 비교
  (map #(println %) (read-file))
  (for [a [1 2 3] b [4 5 6]]
    [a b])

  (seq "pnebjqsalhdgcdzfihvbxywomu")


  (defn find-common [a b]
    (println a b)
    (apply str (last (diff (seq a) (seq b)))))

  (find-common "pnebjqsalhdgcdzfihvbxywomu" "pnebjqsalhdgcdafihvbxywomu")

  (let [ids (read-file)]
    (for [a ids
          b ids
          :when  (diff-one-char? a b)]
      (find-common a b)))

  "pnebjqsalrdnckzfihvtxysomg"
  "prebjqsalrdnckzfihvtxysomg"
  "pebjqsalrdnckzfihvtxysomg"
  "pebjqsalrdnckzfihvtxysomg"

  '())

