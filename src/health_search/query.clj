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

(defn expand-func
  "query expansion function using Dice-coefficient"
  ([func query-terms document-terms] (expand-func func query-terms document-terms (apply merge (map #(hash-map % 1) query-terms))))
  ([func query-terms document-terms expanded-terms]
    (cond
      (empty? query-terms) expanded-terms
      :else
        ; only append a term to the query when the two terms are dependent
        (recur func (rest query-terms) document-terms
          (into expanded-terms (apply merge
            (for [term document-terms
             :let [expand-term {term (func (first query-terms) term document-terms)}]
             :when (> (func (first query-terms) term document-terms) (model/nil-or-zero? (get expanded-terms term)))]
            expand-term)))))))

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
          :query (q/query-string :query query :default_operator "OR") :size 20)
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
  "given a query, expand it using a combination of <func> probability using documents from a standard search and a medical vocabulary"
  [func query]
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
        expanded-query (expand-func func (str/split query #" ") (remove-words-from-sentence (flatten (distinct documents)) model/stopwords))
        medical-term (model/chv-term query)]
        (println "expanded using" (count (remove-words-from-sentence (flatten (distinct documents)) model/stopwords)) "terms in" (count documents) "documents")
        ;; take the 10 best terms from the expanded query
        (println "expanded terms:" (take 10 (sort-by val > expanded-query)))
        (let [expanded-query (keys (into {} (take 10 (sort-by val > expanded-query))))]
          (cond
            ;; the query didn't get expanded but a medical replacement was found
            (and (empty? expanded-query) (not (nil? medical-term))) (apply str [query medical-term])
            ;; there was no medical term replacement
            (nil? medical-term) (str/join #" " expanded-query)
            ;; the query couldn't get expanded at all
            (empty? expanded-query) query
            :else
              ;; there was a medical term replacement and the query was expanded
              (str/join #" " (flatten [expanded-query medical-term]))))))

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
