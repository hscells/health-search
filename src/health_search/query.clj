(ns health-search.query
  (:require [health-search.connection             :as connection]
            [clojurewerkz.elastisch.rest          :as esr]
            [clojurewerkz.elastisch.rest.document :as esd]
            [clojurewerkz.elastisch.query         :as q]
            [clojurewerkz.elastisch.rest.response :as esrsp]
            [clojure.pprint :as pp]))

(defn search
  [query]
  (println "query:" query)
  (let [conn  (esr/connect (connection/config :host))
        res   (esd/search conn "health-search" "document" :query (q/query-string :query query :default_operator "AND"))
        n     (esrsp/total-hits res)
        hits  (esrsp/hits-from res)]
    (println "hits: " n)
    (pp/pprint hits)))
