(ns health-search.model
  "Provide statistical and vocabulary functions relating to the models used in the application"
  (:require [clojure.edn                          :as edn]
            [clojure.string                       :as str]
            [clojure.data.csv                     :as csv]
            [clojurewerkz.elastisch.rest          :as esr]
            [clojurewerkz.elastisch.rest.document :as esd]
            [clojurewerkz.elastisch.query         :as q]
            [health-search.connection             :as connection]))

;; inputs used to tune the models
(def inputs
  "read the properties from the model.edn file and make them available in the program"
  (edn/read-string (slurp "model.edn")))

;; probability related functions
(defn prob
  "calculate the probability of n gram x in terms n"
  ([x n]
    (cond
      (zero? (count n)) 0.1
      :else
        (float (/ (count (filter #{x} n)) (count n)))))
  ([x y n]
    (cond
      (zero? (count n)) 0.1
      :else
        (float (/ (+ (count (filter #{x} n)) (count (filter #{y} n))) (count n))))))

(defn pmi
  "implementation of Pointwise Mututal Information"
  [a b n]
  (cond
    (zero? (* (prob a n) (prob b n))) 0.1
    :else
    (Math/log (/ (prob a b n) (* (prob a n) (prob b n))))))

(defn emim
  "implementation of Expected Mutual Information Measure"
  [a b n]
  (* (prob a b n) (pmi a b n)))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

  (defn nil-or-zero?
    [n]
    (cond
      (nil? n) 0.000000000000001
      :else n))

(def N (inputs :N))

(defn tfc
  [term]
  "get the tf value for a term in the index"
  (let [conn  (esr/connect (connection/config :host))
        res   (esd/count conn (connection/config :index-name) "document" (q/term :text term))]
      (let [c (get res :count)]
        (cond
          (zero? c) 0.1
          :else c))))
(def tfc-memoize (memoize tfc))

(defn Pn
  [t]
  (/ (tfc-memoize t) N))

(defn bo1
  [_ t n]
  (* (prob t n) (+ (log2 (/ (+ 1 (Pn t)) (Pn t))) (log2 (+ 1 (Pn t))))))

(def stopwords (into '() (edn/read-string (slurp "data/stopwords.edn"))))

;; Vocabulary related functions
(def chv
  "access terms in the Consumer Health Vocabulary"
  (let [data (csv/read-csv (slurp "data/CHV_concepts_terms.tsv") :separator \tab)]
    (apply merge (map #(hash-map (nth % 1) (nth % 3)) data))))

(defn chv-term
  "perform a lookup of term in the chv dataset"
  [term]
  (get chv term))


;; the following takes inspiration from http://maroo.cs.umass.edu/pdf/IR-651.pdf
(defn h_k
  "The function h_k which indicates the `confidence` that concept c_i belongs to class KC
  For now, it just uses log normalised tf."
  [c_i document-terms]
    (Math/log (+ 1 (prob c_i document-terms))))

(defn weight-concept
  "weight a concept inside a given query"
  [concept query document-terms]
  (float (/ (* 10000 (/ (h_k concept document-terms) (reduce + (map #(h_k % document-terms) (str/split query #" "))))) 1000)))
