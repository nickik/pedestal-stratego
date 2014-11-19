(ns pedestal-stratego.service
  (:require [io.pedestal.http :as bootstrap]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.http.route.definition :refer [defroutes]]
            [ring.util.response :as ring-resp]
            [pedestal-stratego.game :as g]
            [clojure.pprint :as p]
            [pedestal-stratego.field :as f]))

(defmacro dbg [x] `(let [x# ~x] (do (println '~x "=") (p/pprint x#)) x#))

(defn about-page
  [request]
  (ring-resp/response (format "Clojure %s - served from %s"
                              (clojure-version)
                              (route/url-for ::about-page))))

(defn home-page
  [request]
  (ring-resp/response "Hello World!"))

(defn get-games [request]
  (ring-resp/response  (mapv (fn [game-id]
                              {:url ((url-for) ::get-game :params {:id game-id})})
                             (keys @g/games))))

(defn create-game [request]
  (println "create game")
  (let [id (g/creat-game g/games g/counter 1 2)]
    (ring-resp/response
     {:url
      ((url-for) ::get-game :params {:id id})})))

(defn get-game [request]
  (let [id (get-in request [:path-params :id])]
    (ring-resp/response (g/get-game g/games (Integer/parseInt id)))))

(defn get-index [request]
  (let [id (Integer/parseInt (get-in request [:path-params :id]))
        index (Integer/parseInt (get-in request [:path-params :index]))]
    (ring-resp/response
     (get-in @g/games [id :field index]))))

(defn make-move [request]
  (let [id (Integer/parseInt (get-in request [:path-params :id]))
        index (Integer/parseInt (get-in request [:path-params :index]))

        from-piece-moves (:possible-move (f/get-piece (:field (g/get-game g/games id)) index))

        move {:from index
              :to (some #{(:to (:edn-params request))} from-piece-moves)}]

    (println "from-piece-moves")
    (p/pprint from-piece-moves)
    (println (some #{(:to (:edn-params request))} from-piece-moves))

    (ring-resp/response (when (:to move)
                          (do
                            (g/execute-move g/games id move)
                            (g/add-move g/games id move))))))

(defn get-moves [request]
  (let [id (Integer/parseInt (get-in request [:path-params :id]))]
    (ring-resp/response
     (:moves (g/get-game g/games id)))))

(defn url-for []
  (route/url-for-routes routes))

(defroutes routes
  [[["/" {:get home-page} ^:interceptors [(body-params/body-params) bootstrap/html-body]
     ["/about" {:get about-page}]

     ["/game" {:get get-games
               :post create-game}
      ["/:id" {:get get-game}
       ["/moves" {:get get-moves}]
       ["/:index" {:get get-index
                   :put make-move}]]]]]])


;; Consumed by pedestal-stratego.server/create-server
;; See bootstrap/default-interceptors for additional options you can configure
(def service {:env :prod
              ;; You can bring your own non-default interceptors. Make
              ;; sure you include routing and set it up right for
              ;; dev-mode. If you do, many other keys for configuring
              ;; default interceptors will be ignored.
              ;; :bootstrap/interceptors []
              ::bootstrap/routes routes

              ;; Uncomment next line to enable CORS support, add
              ;; string(s) specifying scheme, host and port for
              ;; allowed source(s):
              ;;
              ;; "http://localhost:8080"
              ;;
              ;;::bootstrap/allowed-origins ["scheme://host:port"]

              ;; Root for resource interceptor that is available by default.
              ::bootstrap/resource-path "/public"

              ;; Either :jetty, :immutant or :tomcat (see comments in project.clj)
              ::bootstrap/type :jetty
              ;;::bootstrap/host "localhost"
              ::bootstrap/port 8080})

