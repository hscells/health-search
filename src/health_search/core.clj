(ns health-search.core
  (:gen-class)
  (:require [health-search.index  :as index]
            [health-search.query  :as query]
            [clojure.string       :as string]))

(def usage
  (->> ["Health serach engine for INB344 at QUT"
        "Harry Scells 2015"
        "Usage: health-search [action] [options]"
        ""
        "Actions:"
        "  index    Index a corpus"
        "  query    Query for a string"
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
          "query" (query/search (first options))
          (exit 1 usage)))))
