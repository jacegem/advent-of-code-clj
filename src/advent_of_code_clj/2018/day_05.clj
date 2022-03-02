(ns advent-of-code-clj.2018.day-05
  (:require [clojure.string :as string]))

;; --- Day 5: Alchemical Reduction ---
;; https://adventofcode.com/2018/day/5

(defn read-file [year day]
  (-> (format "resources/%d/input_%02d.txt" year day)
      slurp
      string/split-lines))

(defn react?
  "반응 여부 확인"
  {:test #(do (assert (= (react? "A") nil))
              (assert (= (react? "Aa") '("Aa"))))}
  [polymers]
  (re-seq #"Aa|Bb|Cc|Dd|Ee|Ff|Gg|Hh|Ii|Jj|Kk|Ll|Mm|Nn|Oo|Pp|Qq|Rr|Ss|Tt|Uu|Ww|Vv|Xx|Yy|Zz|aA|bB|cC|dD|eE|fF|gG|hH|iI|jJ|kK|lL|mM|nN|oO|pP|qQ|rR|sS|tT|uU|wW|vV|xX|yY|zZ"
          polymers))
;; (test #'react?)

(defn destroy
  "반응 대상 제거"
  {:test #(do (assert (= (destroy "Aa") ""))
              (assert (= (destroy "AaBbC") "C"))
              (assert (= (destroy "AaBbdCcD") "dD")))}
  [polymers]
  (string/replace polymers
                  #"Aa|Bb|Cc|Dd|Ee|Ff|Gg|Hh|Ii|Jj|Kk|Ll|Mm|Nn|Oo|Pp|Qq|Rr|Ss|Tt|Uu|Ww|Vv|Xx|Yy|Zz|aA|bB|cC|dD|eE|fF|gG|hH|iI|jJ|kK|lL|mM|nN|oO|pP|qQ|rR|sS|tT|uU|wW|vV|xX|yY|zZ"
                  ""))
;; (test #'destroy)

(defn react-all
  "모든 반응 대상 제거"
  {:test #(do (assert (= (react-all "Aa") ""))
              (assert (= (react-all "AaBbdCcD") "")))}
  [s]
  (let [polymers (atom s)]
    (while (react? @polymers)
      (reset! polymers (destroy @polymers)))
    @polymers))
;; (test #'react-all)

(defn part-1 []
  (let [input (first (read-file 2018 5))]
    (-> (react-all input)
        (count))))

(defn remove-unit-count
  "unit 제거 후 count 반환"
  {:test #(do (assert (= (remove-unit-count "AaB" \A) 1))
              (assert (= (remove-unit-count "AaB" \b) 0)))}
  [polymers unit]
  (-> (string/replace polymers (re-pattern (str "(?i)" unit)) "")
      (react-all)
      count))
;; (test #'remove-unit-count)

(defn part-2 []
  (let [chars (map char (range (int \A) (inc (int \Z))))
        input (first (read-file 2018 5))]
    (->> (map #(remove-unit-count input %) chars)
         (apply min))))

(comment
  (part-1)
  (part-2)
  (test #'react?)
  (test #'destroy)
  (test #'react-all)
  (test #'remove-unit-count)
  '())


;; (read-file 2018 5)
;; (-> (map (fn [unit] (remove polymers unit) (map char (range 65 91)))))
;; (map char (range 65 91))
;; (re-seq #"Aa|Bb|Cc|Dd|Ee|Ff|Gg|Hh|Ii|Jj|Kk|Ll|Mm|Nn|Oo|Pp|Qq|Rr|Ss|Tt|Uu|Ww|Vv|Xx|Yy|Zz|aA|bB|cC|dD|eE|fF|gG|hH|iI|jJ|kK|lL|mM|nN|oO|pP|qQ|rR|sS|tT|uU|wW|vV|xX|yY|zZ"
;;         "kKpPcCZQqzyYvVxXVfY")
;; (string/replace "kKpPcCZQqzyYvVxXVfY"
;;                 #"Aa|Bb|Cc|Dd|Ee|Ff|Gg|Hh|Ii|Jj|Kk|Ll|Mm|Nn|Oo|Pp|Qq|Rr|Ss|Tt|Uu|Ww|Vv|Xx|Yy|Zz|aA|bB|cC|dD|eE|fF|gG|hH|iI|jJ|kK|lL|mM|nN|oO|pP|qQ|rR|sS|tT|uU|wW|vV|xX|yY|zZ"
;;                 "")
;; (clojure.string/replace "kKpPcCZQqzyYvVxXVfY" #"red" "")
;; ;; (def polymers "kKpPcCZQqzyYvVxXVfY")
;; (def polymers
;;   (atom (first (read-file 2018 5))))
;; (string/replace "kKpPcCZQqzyYvVxXVfY" '("kK") "")
