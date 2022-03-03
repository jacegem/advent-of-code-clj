(ns advent-of-code-clj.example.spec.spec
  (:require [clojure.spec.alpha :as s]
            [failjure.core :as f]
            [clojure.spec.test.alpha :as stest]))


(s/fdef digits
  :args (s/cat :just-an-int int?)
  :ret (s/coll-of char? :kind set? :min-count 1))
(defn digits
  "Takes just an int and returns the set of its digit characters."
  [just-an-int]
  (into #{} (str just-an-int)))


(digits 12)
(stest/instrument `digits)
(stest/check `digits)

(defn stringer-bell
  "Prints a string and rings bell."
  [s]
  {:pre [(s/valid? (s/nilable string?) s)]}
  (println s "\007"))


(defn stringer-bell [s]
  (s/assert (s/nilable string?) s)
  (println s "\007"))

(stringer-bell 12)

(s/check-asserts true)


;; https://adambard.com/blog/domain-modeling-with-clojure-spec/
(s/def ::rss-feed
  (s/keys
   :req-un [::title ::description ::link-uri ::items]))

(s/def ::feed-item
  (s/keys
   :req-un [::title ::description ::link-uri ::comments-uri ::pub-date]))

(s/def ::title string?)
(s/def ::description string?)
(s/def ::link-uri uri?)
(s/def ::comments-uri uri?)
(s/def ::items (s/coll-of ::feed-item))
(s/def ::blacklist (s/coll-of #(instance? java.util.regex.Pattern %)))

(s/def ::content string?)
(s/def ::feed-item-with-content
  (s/and ::feed-item
         (s/keys :req-un [::content])))

;; Defining the process
(s/def ::fetched-item-result
  (s/or :ok ::feed-item-with-content
        :error ::feed-item))

(s/def ::try-fetch-item-content
  (s/fspec
   :args (s/cat :blacklist ::blacklist
                :item ::feed-item)
   :ret ::fetched-item-result))

(s/def ::try-fetch-items
  (s/fspec
   :args (s/cat :blacklist ::blacklist
                :feed ::rss-feed)
   :ret (s/coll-of ::fetched-item-result)))

(s/def ::get-rss-feed
  (s/fspec
   :args (s/cat :uri uri?)
   :ret (s/or
         :ok ::rss-feed
         :error f/failed?)))


(comment
  (require '[clojure.spec.test.alpha :as stest])
  (stest/instrument)

  '())