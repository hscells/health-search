(ns health-search.connection
  "Utility class which provides access to configuration settings at run time of the application"
  (:require [clojure.edn :as edn]))

(def config
  "read the properties from the config.edn file and make them available in the program"
  (edn/read-string (slurp "config.edn")))
