(defproject trajectory "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [net.mikera/core.matrix.stats "0.7.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [seesaw "1.4.5"]]
  :main ^:skip-aot trajectory.core
  :target-path "target/%s"
  :profiles {:dev {:plugins [[com.jakemccrary/lein-test-refresh "0.21.1"]]}
             :uberjar {:aot :all}})
