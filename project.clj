(defproject health-search "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clojurewerkz/elastisch "2.1.0"]]
  :main ^:skip-aot health-search.core
  :target-path "target/%s"
  :plugins [[codox "0.8.13"]]
  :codox {:output-dir "hscells.github.io/health-search/doc"}
  :profiles {:uberjar {:aot :all}})
