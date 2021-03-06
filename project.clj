(defproject pedestal-stratego "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [io.pedestal/pedestal.service "0.3.1"]

                 [io.pedestal/pedestal.jetty "0.3.1"]
                 ;; [io.pedestal/pedestal.tomcat "0.3.1"]
                 ;; [io.pedestal/pedestal.immutant "0.3.1"]

                 [ch.qos.logback/logback-classic "1.1.2" :exclusions [org.slf4j/slf4j-api]]
                 [org.slf4j/jul-to-slf4j "1.7.7"]
                 [org.slf4j/jcl-over-slf4j "1.7.7"]
                 [org.slf4j/log4j-over-slf4j "1.7.7"]

                 [ns-tracker "0.2.2"]
                 [prismatic/schema "0.2.6"]
                 [org.clojure/tools.reader "0.8.8"]
                 [lein-light-nrepl "0.1.0"]

                 [com.datomic/datomic-free "0.9.5067" :exclusions [joda-time]]
                 [geheimtur "0.1.2"]
                 [hiccup "1.0.5"]

                 ]
  :min-lein-version "2.0.0"
  :repl-options {:nrepl-middleware [lighttable.nrepl.handler/lighttable-ops]}
  :resource-paths ["config", "resources"]
  :profiles {:dev {:aliases {"run-dev" ["trampoline" "run" "-m" "pedestal-stratego.server/run-dev"]}
                   :dependencies [[io.pedestal/pedestal.service-tools "0.3.1"]]}}
  :main ^{:skip-aot true} pedestal-stratego.server)

