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
  ([x n]
  (float (/ (count (filter #{x} n)) (count n))))
  ([x y n]
  (float (/ (+ (count (filter #{x} n)) (count (filter #{y} n))) (count n)))))

(defn pmi
  "implementation of Pointwise Mututal Information"
  [a b n]
  (cond
    (zero? (* (prob a n) (prob b n))) 0
    :else
    (Math/log (/ (prob a b n) (* (prob a n) (prob b n))))))

(defn emim
  "implementation of Expected Mutual Information Measure"
  [a b n]
  (* (prob a b n) (pmi a b n)))

(defn expand-emim
  "query expansion function using Dice-coefficient"
  ([query-terms document-terms] (expand-emim query-terms document-terms query-terms))
  ([query-terms document-terms expanded-terms]
    (cond
      (empty? query-terms) expanded-terms
      :else
        ; only append a term to the query when the two terms are dependent
        (recur (rest query-terms) document-terms
          (conj expanded-terms (for [term document-terms :let [expand-term term] :when (> 0.5 (emim (first query-terms) term document-terms))] expand-term))))))

(defn print-search
  "print the results from the serach nicely"
  [hits scores titles ids]
  (dotimes [i (count scores)]
    (println (nth scores i) (nth titles i) (nth ids i))))

(defn search
  "perform a basic search using query strings"
  [query]
  (let [conn  (esr/connect (connection/config :host))
        res   (esd/search conn "health-search" "document"
          :query (q/query-string :query query :default_operator "OR"))
        n     (esrsp/total-hits res)
        hits  (esrsp/hits-from res)
        ids (map #(get % :_id) hits)
        scores (map #(get % :_score) hits)
        titles (map #(get (get % :title) :_source) hits)]
    (hash-map :hits hits :scores scores :titles titles :ids ids)))

(defn get-terms
  ([document-terms] (get-terms document-terms (list)))
  ([document-terms terms]
    (cond
      (empty? document-terms) terms
      :else
        (recur (rest document-terms) (conj terms (get (first document-terms) :token))))))

(defn expanded-search
  "perform a search using an expanded query. The expanded query is calculated using terms from similar documents"
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
    (doseq [document documents]
      (println (expand-emim (str/split query #" ") document)))))
    ; (search (expand-emim (str/split query #" ") doc-terms))))
