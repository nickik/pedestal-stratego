(ns pedestal-stratego.server
  (:gen-class) ; for -main method in uberjar
  (:require [io.pedestal.http :as server]
            [pedestal-stratego.service :as service]
            [ns-tracker.core :refer [ns-tracker]]))

;; This is an adapted service map, that can be started and stopped
;; From the REPL you can call server/start and server/stop on this service
(defonce runnable-service (server/create-server service/service))

(def modified-namespaces (ns-tracker "src"))

#_(def serv (run-dev))

(defn run-dev
  "The entry-point for 'lein run-dev'"
  [& args]
  (println "\nCreating your [DEV] server...")
  (-> service/service
      (merge {:env :dev
              ::server/join? false
              ::server/routes (fn []
                                (doseq [ns-sym (modified-namespaces)]
                                  (require ns-sym :reload))
                                service/routes)
              ::server/allowed-origins {:creds true :allowed-origins (constantly true)}})
      ;; Wire up interceptor chains
      server/default-interceptors
      server/dev-interceptors
      server/create-server
      server/start))

(defn -main
  "The entry-point for 'lein run'"
  [& args]
  (println "\nCreating your server...")
  (server/start runnable-service))

