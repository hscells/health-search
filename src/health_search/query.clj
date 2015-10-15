(ns health-search.query
  "Perform various query and search related actions"
  (:require [health-search.connection             :as connection]
            [health-search.model                  :as model]
            [clojurewerkz.elastisch.rest          :as esr]
            [clojurewerkz.elastisch.rest.document :as esd]
            [clojurewerkz.elastisch.query         :as q]
            [clojurewerkz.elastisch.rest.response :as esrsp]
            [clojure.string                       :as str]
            [clojure.pprint                       :as pp]))

(defn expand-emim
  "query expansion function using Dice-coefficient"
  ([query-terms document-terms] (expand-emim query-terms document-terms query-terms))
  ([query-terms document-terms expanded-terms]
    (cond
      (empty? query-terms) (distinct (flatten expanded-terms))
      :else
        ; only append a term to the query when the two terms are dependent
        (recur (rest query-terms) document-terms
          ; use the emim formula to filter terms above the emim-prob value
          (conj expanded-terms
            (for [term document-terms :let [expand-term term] :when (> (model/emim (first query-terms) term document-terms) (model/inputs :emim-prob))] expand-term))))))

(defn print-search
  "print the results from the serach nicely"
  [search-results]
  (dotimes [i (count (get search-results :scores))]
    (println (nth (get search-results :scores) i) (nth (get search-results :titles) i) (nth (get search-results :ids) i))))

(defn search
  "perform a basic search using query strings"
  [query]
  (let [conn  (esr/connect (connection/config :host))
        res   (esd/search conn (connection/config :index-name) "document"
          :query (q/query-string :query query :default_operator "OR"))
        n     (esrsp/total-hits res)
        hits  (esrsp/hits-from res)
        ids (map #(get % :_id) hits)
        scores (map #(get % :_score) hits)
        titles (map #(get (get % :title) :_source) hits)]
    (hash-map :hits hits :scores scores :titles titles :ids ids)))

(defn get-terms
  "given a set of terms from a set of documents from a search, return just the terms themselves and not the associated data"
  ([document-terms] (get-terms document-terms (list)))
  ([document-terms terms]
    (cond
      (empty? document-terms) terms
      :else
        (recur (rest document-terms) (conj terms (get (first document-terms) :token))))))

(defn expand-query
  "given a query, expand it using a combination of emim probability using documents from a standard search and a medica vocabulary"
  [query]
  (let [conn  (esr/connect (connection/config :host))
        results (search query)
        hits (get results :hits)
        doc-source (map #(esd/analyze conn (get (get % :_source) :text) :analyzer {:custom_health {
           :type         "custom"
           :tokenizer    "standard"
           :char_filter  "html_strip"
           :filter       ["standard" "lowercase" "snowball"]}}) hits)
        documents (map #(get-terms %) (map #(get % :tokens) doc-source))
        expanded-query (str/join " " (distinct (flatten (for [document documents :let [terms (expand-emim (str/split query #" ") document)]] terms))))
        medical-term (model/chv-term query)]
        ;; we have the list of terms which emim found were similar in similar documents
        ;; now we look up to see if the query is in the CHV, and if it is, replace it in the expanded query
        (cond
          ;; the query didn't get expanded but a medical replacement was found
          (and (str/blank? expanded-query) (not (nil? medical-term))) medical-term
          ;; there was no medical term replacement
          (nil? medical-term) expanded-query
          ;; the query couldn't get expanded at all
          (str/blank? expanded-query) query
          :else
            ;; there was a medical term replacement and the query was expanded
            (str/join #" " expanded-query query medical-term))))
