(ns health-search.index
  (:require [health-search.connection :as connection]
            [clojurewerkz.elastisch.rest :as esr]
            [clojurewerkz.elastisch.rest.document :as esd]
            [clojure.java.io :as io]))

(defn add-to-index
  [name id conn]
  (println name)
  (esd/create conn "health-search" "document" {:name name :text (slurp name) :timestamp (/ (System/currentTimeMillis) 1000)}))

(defn index-collection
  [options]
  ; connect to the elastic search instance
  (let [corpus (first options) conn (esr/connect (connection/config :host)) id 0]
    (cond
      (nil? corpus) nil
      :else
      (doseq [file (file-seq (io/file corpus))]
        (cond
          (.isDirectory file) nil
          :else (add-to-index (.getPath file) (str (inc id)) conn))))))
          ; (add-to-index (.getName file) (inc id) conn))))))
