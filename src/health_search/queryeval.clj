(ns health-search.queryeval
  "Used to generate a results file compaitble with trec_eval"
  (:require [clojure.string       :as str]
            [clojure.edn          :as edn]
            [health-search.query  :as query]
            [health-search.model  :as model]))

(defn create-results-file
  "Create the results file needed for trec_eval to evaluate"
  [filename query id relevance iter]
    (spit filename (str (str/replace query #":" "") \tab "Q0" \tab (str (str/replace id #".html" "")) \tab iter \tab relevance \tab "banana" \newline) :append true))

(defn bulk-search
  "given an input file, create an internal results file representation to be written out later"
  ([input]
    (let [data (reverse (into (sorted-map) (edn/read-string (slurp input))))]
      (bulk-search data (list))))
  ([data hits]
    (cond
      (empty? data) hits
      :else
      (recur
        (rest data)
        (conj hits (pmap #(hash-map :query (key (first data)) :id (get % :_id) :relevance (get % :_score)) (get (query/search (query/expand-query model/bo1 (val (first data)))) :hits)))))))

(defn search
  "Performs a bulk search on input and outputs the results in output, or results.dat if none specified"
  ([input] (search input "results.dat"))
  ([input output]
    (spit output "")
      (doseq [query-result (bulk-search input)]
          (doseq [qrel query-result]
            (create-results-file output (get qrel :query) (get qrel :id) (get qrel :relevance) 0)))))
