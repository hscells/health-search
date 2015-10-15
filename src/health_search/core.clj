(ns health-search.core
  "Entrypoint to the application, providing command line utilities"
  (:gen-class)
  (:require [health-search.index        :as index]
            [health-search.query        :as query]
            [health-search.queryeval    :as queryeval]
            [clojure.string             :as string]))

(def usage
  (->> ["Health serach engine for INB344 at QUT"
        "Harry Scells 2015"
        "Usage: health-search [action] [options]"
        ""
        "Actions:"
        "  index        Index a corpus"
        "  query        Query for a string"
        "  bulk-query   Use a file for input as the search and produce a file with qrel output"
        ""
        "Please refer to the manual page for more information."]
       (string/join \newline)))

(defn exit [status msg]
 (println msg)
 (System/exit status))

(defn -main
  [& args]
  (cond
    (= 0 (count args)) (exit 1 usage)
    :else
      (let [arg (first args) options (rest args)]
        (case arg
          "index" (index/index-collection options)
          ; "query" (apply query/print-search (query/search (first options)))
          ; "query" (println (query/expanded-search (first options)))
          "query" (query/print-search (query/search (query/expand-query (first options))))
          "bulk-query" (queryeval/search (first options) (second options))
          (exit 1 usage)))))
