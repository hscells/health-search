(ns health-search.queryeval
  (:require [clojure.string       :as str]
            [clojure.edn          :as edn]
            [health-search.query  :as query]))

(defn create-qrel
  "Create the file needed for trec_eval to evaluate"
  [filename query id relevance iter]
    (spit filename (str (str/replace query #":" "") \tab "Q0" \tab (str (str/replace id #".html" "")) \tab iter \tab relevance \tab "banana" \newline) :append true))

(defn bulk-search
  ([input]
    (let [data (reverse (into (sorted-map) (edn/read-string (slurp input))))]
      (bulk-search data (list))))
  ([data hits]
    (cond
      (empty? data) hits
      :else
      (recur
        (rest data)
        (conj hits (map #(hash-map :query (key (first data)) :id (get % :_id) :relevance (get % :_score)) (get (query/search (val (first data))) :hits)))))))

(defn search
  "Performs a bulk search on input and outputs the results in output, or results.qrel if none specified"
  ([input] (search input "results.qrel"))
  ([input output]
    (spit output "")
      (doseq [query-result (bulk-search input)]
          (doseq [qrel query-result]
            (create-qrel output (get qrel :query) (get qrel :id) (get qrel :relevance) 0)))))
