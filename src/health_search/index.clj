(ns health-search.index
  "Functions related to creating an elastic search index"
  (:require [health-search.connection             :as connection]
            [health-search.model                  :as model]
            [clojurewerkz.elastisch.rest          :as esr]
            [clojurewerkz.elastisch.rest.document :as esd]
            [clojurewerkz.elastisch.rest.index    :as esi]
            [clojure.string                       :as str]
            [clojure.java.io                      :as io]
            [net.cgrand.enlive-html               :as html]))

;; This code is credited to:
;; http://shenfeng.me/using-tagsoup-extract-text-from-html.html
(defn- emit-str [node]
   (cond (string? node) node
         (and (:tag node)
              (not= :script (:tag node))) (emit-str (:content node))
         (seq? node) (map emit-str node)
         :else ""))

 (defn extract-text [html]
   (when html
     (let [r (html/html-resource (java.io.StringReader. html))]
       (str/trim (apply str (flatten (emit-str r)))))))

(defn add-to-index
  "Add a document to the index with a name, an id with the elstic search connection"
  [name id conn]
  (println name)
  (let [html (slurp name)]
    (esd/put conn (connection/config :index-name) "document" id {:title (get (first (html/select (html/html-resource (java.io.StringReader. html)) [:title])) :content) :text (extract-text html)})))

(defn index-collection
  "take a directory and index it on elastic search"
  [options]
  ; connect to the elastic search instance
  (let [corpus (first options) conn (esr/connect (connection/config :host))]
    (esi/create conn (connection/config :index-name)
            :settings {:index {:analysis {:analyzer {:custom_health {
                                                                     :type         "custom"
                                                                     :tokenizer    "standard"
                                                                     :char_filter  "html_strip"
                                                                     :filter       ["standard" "lowercase" "snowball"]}}}}}
            :mappings {
              :document {
                :_all {:enabled "true"}
                :properties {
                  :text  {:type "string" :analyzer "custom_health" :search_analyzer "custom_health" :store "no" :index "analyzed" :similarity {
                    :type "BM25"
                    :b    (model/inputs :bm25-b)
                    :k1   (model/inputs :bm25-k1)}}
                  :title {:type "string"}}}})
    (cond
      (nil? corpus) nil
      :else
      (doseq [file (file-seq (io/file corpus))]
        (cond
          (.isDirectory file) nil
          :else (add-to-index (.getPath file) (.getName file) conn))))))
