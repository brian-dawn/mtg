(defproject mtg "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [clj-fuzzy "0.3.1"]
                 ;;[clojurewerkz/elastisch "2.2.0-beta4"] LAWL OVERKILL!!!
                 [cheshire "5.5.0"]]
  :main ^:skip-aot mtg.core
  :target-path "target/%s"
  ;;:repl-options {:init (clojure.main/repl :print pprint)}
  :profiles {:uberjar {:aot :all}})
