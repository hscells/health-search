(ns health-search.connection
  (:require [clojure.edn :as edn]))

(def config
  "read the properties from the config.edn file and make them available in the program"
  (edn/read-string (slurp "config.edn")))
