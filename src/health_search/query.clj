(ns health-search.query
  "Perform various query and search related actions"
  (:require [health-search.connection             :as connection]
            [health-search.model                  :as model]
            [clojurewerkz.elastisch.rest          :as esr]
            [clojurewerkz.elastisch.rest.document :as esd]
            [clojurewerkz.elastisch.query         :as q]
            [clojurewerkz.elastisch.rest.response :as esrsp]
            [clojure.string                       :as str]
            [clojure.set                          :as set]
            [clojure.pprint                       :as pp]))

(defn remove-words-from-sentence [sentence words]
    (into [] (set/difference (into #{} sentence) words)))

(defn expand-emim
  "query expansion function using Dice-coefficient"
  ([query-terms document-terms] (expand-emim query-terms document-terms query-terms))
  ([query-terms document-terms expanded-terms]
    (cond
      (empty? query-terms) (distinct (remove-words-from-sentence (flatten expanded-terms) model/stopwords))
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
    (println (nth (get search-results :scores) i) (nth (get search-results :ids) i))))

(defn search
  "perform a basic search using query strings"
  [query]
  (let [conn  (esr/connect (connection/config :host))
        res   (esd/search conn (connection/config :index-name) "document"
          :query (q/query-string :query query :default_operator "OR") :size 5)
        n     (esrsp/total-hits res)
        hits  (esrsp/hits-from res)
        ids (map #(get % :_id) hits)
        scores (map #(get % :_score) hits)]
    (hash-map :hits hits :scores scores :ids ids)))

(defn get-terms
  "given a set of terms from a set of documents from a search, return just the terms themselves and not the associated data"
  ([document-terms] (get-terms document-terms (list)))
  ([document-terms terms]
    (cond
      (empty? document-terms) terms
      :else
        (recur (rest document-terms) (conj terms (get (first document-terms) :token))))))

(defn get-document-terms
  [query]
  (let [conn  (esr/connect (connection/config :host))
        results (search query)
        hits (get results :hits)
        doc-source (map #(esd/analyze conn (get (get % :_source) :text) :analyzer {:custom_health {
           :type         "custom"
           :tokenizer    "standard"
           :char_filter  "html_strip"
           :filter       ["standard" "lowercase" "snowball"]}}) hits)
        documents (map #(get-terms %) (map #(get % :tokens) doc-source))]
      (flatten documents)))

(defn expand-query
  "given a query, expand it using a combination of emim probability using documents from a standard search and a medical vocabulary"
  [query]
  (println "expanding" query)
  (let [conn  (esr/connect (connection/config :host))
        results (search query)
        hits (get results :hits)
        doc-source (map #(esd/analyze conn (get (get % :_source) :text) :analyzer {:custom_health {
           :type         "custom"
           :tokenizer    "standard"
           :char_filter  "html_strip"
           :filter       ["standard" "lowercase" "snowball"]}}) hits)
        documents (map #(get-terms %) (map #(get % :tokens) doc-source))
        expanded-query (distinct (flatten (for [document documents :let [terms (expand-emim (str/split query #" ") (remove-words-from-sentence document model/stopwords))]] terms)))
        medical-term (model/chv-term query)]
        ;; we have the list of terms which emim found were similar in similar documents
        ;; now we look up to see if the query is in the CHV, and if it is, replace it in the expanded query
        (cond
          ;; the query didn't get expanded but a medical replacement was found
          (and (empty? expanded-query) (not (nil? medical-term))) (apply str [query medical-term])
          ;; there was no medical term replacement
          (nil? medical-term) (str/join #" " (remove-words-from-sentence expanded-query model/stopwords))
          ;; the query couldn't get expanded at all
          (empty? expanded-query) (str/join #" " (remove-words-from-sentence query model/stopwords))
          :else
            ;; there was a medical term replacement and the query was expanded
            (str/join #" " (remove-words-from-sentence (flatten [expanded-query medical-term]) model/stopwords)))))

(defn weight-query-binary
  "weight-query will take a query and weight it into two classes: KC (key concepts class) and NKC (non-key concepts class)"
  ([query document-terms] (weight-query-binary (str/split query #" ") query {:must [] :should [] :must_not []} document-terms))
  ([terms query mapping document-terms]
    (let [prob-cw (model/weight-concept (first terms) query document-terms)]
      ; (println prob-cw (first terms))
      (cond
        (empty? terms) mapping
        (zero? prob-cw)
          (recur (rest terms) query (assoc-in mapping [:must_not (count (get mapping :must_not))] {:term {:text (str (first terms))}}) document-terms)
        (> prob-cw (model/inputs :concept-weighting))
          (recur (rest terms) query (assoc-in mapping [:must (count (get mapping :must))] {:term {:text (str (first terms))}}) document-terms)
        :else
          (recur (rest terms) query (assoc-in mapping [:should (count (get mapping :should))] {:term {:text (str (first terms))}}) document-terms)))))

(defn weight-query-term
  "weight-query will take a query and weight it based on modified TF"
  ([query document-terms] (weight-query-term (str/split query #" ") query [] document-terms))
  ([terms query mapping document-terms]
    (let [prob-cw (model/weight-concept (first terms) query document-terms)]
      ; (println prob-cw (first terms))
      (cond
        (empty? terms) mapping
        :else
          (recur (rest terms) query (conj mapping {:filter {:term {:text (str (first terms))}} :weight prob-cw}) document-terms)))))

(defn cw-query
  "Perform a Concept Weighted query using a Boolean Query"
  [query]
  (println "running cw-query for" query)
  (let [conn      (esr/connect (connection/config :host))
        doc-terms (get-document-terms query)
        cwq       (weight-query-term query doc-terms)
        res       (esd/search conn (connection/config :index-name) "document" :query {:function_score {:query (q/query-string :query query :default_operator "OR") :functions cwq}} :size 1000)
        hits      (esrsp/hits-from res)
        ids       (map #(get % :_id) hits)
        scores    (map #(get % :_score) hits)]
      (hash-map :hits hits :scores scores :ids ids)))
