(defproject mastermind "0.1.0-SNAPSHOT"
  :description "projet thème 1 mastermind"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot mastermind.main
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[midje "1.8.3" :exclusions [org.clojure/clojure]]
                                  [org.clojure/tools.nrepl "0.2.12"]]
                   :plugins [[lein-midje "3.2.1"]]}
             :midje {}})
