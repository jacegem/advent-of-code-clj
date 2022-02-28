(ns advent-of-code-clj.example.spec.snake
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.spec.test.alpha :as stest]))

;; https://philoskim.github.io/docs/spec/doc/intro.html

(s/fdef snake-case
  :args (s/cat :k keyword?)
  :ret string?)

(defn snake-case
  "Converts lisp-case keyword to snake-case string.
   ex) :get-sock-address => \"get_sock_address\""
  [k]
  (-> (name k) (str/replace "-" "_")))

(snake-case "aBC-asdf")


(stest/instrument)

(s/fdef my-max3
  :args (s/and (s/cat :coll (s/coll-of number?))
               #(every? (fn [num]
                          (< num 10))
                        (:coll %)))
  :ret number?)

(defn my-max3 [coll]
  (apply max coll))

(stest/instrument `my-max3)

(my-max3 [1 2 3 14])


(s/fdef ranged-rand
  :args (s/and (s/cat :start int? :end int?)
               #(< (:start %) (:end %)))
  :ret int?
  :fn (s/and #(>= (:ret %) (-> % :args :start))
             #(< (:ret %) (-> % :args :end))))

(defn ranged-rand
  "Returns random int in range start <= rand < end"
  [start end]
  (+ start (long (rand (- end start)))))

(stest/instrument `ranged-rand)

(s/exercise-fn `ranged-rand 5)

(stest/check `ranged-rand)


(s/exercise (s/cat :ns (s/? string?) :name string?))