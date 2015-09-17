(ns health-search.index
  (:require [health-search.connection :as connection]
            [clojurewerkz.elastisch.rest :as esr]
            [clojurewerkz.elastisch.rest.document :as esd]
            [clojure.java.io :as io]))

(defn add-to-index
  [name id conn]
  (println name)
  (esd/put conn "health-search" "document" id {:name id :body (slurp name)}))

(defn index-collection
  [options]
  ; connect to the elastic search instance
  (let [corpus (first options) conn (esr/connect (connection/config :host)) id 0
        mapping-types {"document" {:properties {
                                    :name {:type "string" :store "yes"}
                                    :text {:type "string" :analyzer "snowball"}}}}]
    (cond
      (nil? corpus) nil
      :else
      (doseq [file (file-seq (io/file corpus))]
        (cond
          (.isDirectory file) nil
          :else (add-to-index (.getPath file) (.getName file) conn))))))
