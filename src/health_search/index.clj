(ns health-search.index
  (:require [health-search.connection :as connection]
            [clojurewerkz.elastisch.rest :as esr]
            [clojurewerkz.elastisch.rest.document :as esd]
            [clojure.java.io :as io]))

(defn add-to-index
  "Add a document to the index with a name, an id with the elstic search connection"
  [name id conn]
  (println name)
  (esd/put conn "health-search" "document" id {:name id :text (slurp name)}))

(defn index-collection
  "take a directory and index it on elastic search"
  [options]
  ; connect to the elastic search instance
  (let [corpus (first options) conn (esr/connect (connection/config :host)) id 0
        mapping-types {"document" {:properties {
                                    :name {:type "string" :store "yes"}
                                    :text {:type "string" :index "BM25"}}}}]
    (cond
      (nil? corpus) nil
      :else
      (doseq [file (file-seq (io/file corpus))]
        (cond
          (.isDirectory file) nil
          :else (add-to-index (.getPath file) (.getName file) conn))))))
