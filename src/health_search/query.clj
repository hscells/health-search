(ns health-search.query
  (:require [health-search.connection             :as connection]
            [clojurewerkz.elastisch.rest          :as esr]
            [clojurewerkz.elastisch.rest.document :as esd]
            [clojurewerkz.elastisch.query         :as q]
            [clojurewerkz.elastisch.rest.response :as esrsp]
            [clojure.string                       :as str]
            [clojure.pprint :as pp]))

(defn prob
  "calculate the probability of n gram x in terms n"
  [x n]
  (/ (count (filter x n)) (count n)))

(defn prob
  "calculate the probability of n grams x and y in terms n"
  [x y n]
  (/ (+ (count (filter x n) (count (filter y n)))) (count n)))

(defn dice
  "implementation of Dice-coefficient"
  [a, b]
  (/ (* 2 (* a b)) (+ a b)))

(defn pmi
  "implementation of Pointwise Mututal Information"
  [a b n]
  (Math/log (/ (prob a b) (* (prob a ) (prob b)))))

(defn emim
  "implementation of Expected Mutual Information Measure"
  [a b n]
  (* (prob a b n) (pmi a b n))

(defn expand-dice
  "query expansion function using Dice-coefficient"
  [terms bag]
  ;; naiive implementation...
  (map dice terms bag))

(defn expand-query
  "given a query string, apply a query expanding function to it"
  [query function]
  (let [terms (str/split query #" ") expanded-query (apply function terms)]
    expanded-query))

(defn search
  [query]
  (println "query:" query)
  (let [conn  (esr/connect (connection/config :host))
        res   (esd/search conn "health-search" "document" :query (q/query-string :query query :default_operator "OR"))
        n     (esrsp/total-hits res)
        hits  (esrsp/hits-from res)]
    (println "hits: " n)
    (pp/pprint hits)))
