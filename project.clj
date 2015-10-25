(defproject health-search "1.0"
  :description "A health-based search engine"
  :url "https://github.com/hscells/health-search"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.csv "0.1.3"]
                 [clojurewerkz/elastisch "2.1.0"]
                 [enlive "1.1.6"]]
  :main ^:skip-aot health-search.core
  :target-path "target/%s"
  :plugins [[codox "0.8.13"]]
  :codox {:output-dir "hscells.github.io/health-search/doc"}
  :profiles {:uberjar {:aot :all}})
