(ns pedestal-stratego.service
  (:require [io.pedestal.http :as bootstrap]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.http.route.definition :refer [defroutes]]
            [io.pedestal.interceptor :as interceptor :refer [defon-request]]
            [ring.util.response :as ring-resp]
            [pedestal-stratego.game :as g]
            [clojure.pprint :as p]
            [pedestal-stratego.field :as f]
            [pedestal-stratego.peer :as d]
            [geheimtur.util.auth :as auth :refer [throw-forbidden get-identity]]
            [geheimtur.interceptor :refer [ interactive guard http-basic]]))

(defmacro dbg [x] `(let [x# ~x] (do (println '~x "=") (p/pprint x#)) x#))

(declare routes)

(defn url-for []
  (route/url-for-routes routes))

(defn about-page
  [request]
  (ring-resp/response (format "Clojure %s - served from %s"
                              (clojure-version)
                              (route/url-for ::about-page))))

(defn user-page [request]
  (let [user (get-in request [:path-params :user])
        e (d/entity? user)
        user (if e
               (d/get-user-by-entity (:datomic-db request) e)
               user)]
    (ring-resp/response (str (d/get-user (:datomic-db request) user)))))

(defn creat-user [request]
  (let [user (:name (:edn-params request))
        pw   (:password (:edn-params request))]
    (ring-resp/response
     (str
      (d/creat-user (:datomic-conn request) user pw)))))

(defn get-games [request]
  (ring-resp/response  (mapv (fn [game-id]
                              {:url ((url-for) ::get-game :params {:id game-id})})
                             (keys @g/games))))

(defn home-page [req]
   (ring-resp/response "This is it, the best webside ever."))

(defn create-game [request]
  (let [id (g/creat-game g/games g/counter (:user (get-identity request)) nil)]
    (ring-resp/redirect
     ((url-for) ::get-game :params {:id id}))))


#_(defn add-start-pos [request]
  (let [id (get-in request [:path-params :id])
        start (:edn-params request)]
    ))

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

    #_(println "from-piece-moves")
    #_(p/pprint from-piece-moves)
    #_(println (some #{(:to (:edn-params request))} from-piece-moves))

    (ring-resp/response (when (:to move)
                          (do
                            (g/execute-move g/games id move)
                            (g/add-move g/games id move))))))

(defn get-moves [request]
  (let [id (Integer/parseInt (get-in request [:path-params :id]))]
    (ring-resp/response
     (:moves (g/get-game g/games id)))))

(defn is-part-of-game? [context]
  (let [id (get-in context [:path-params :id])
        game (g/get-game g/games (Integer/parseInt id))
        player1 (:player1 game)
        player2 (:player2 game)
        user (:user (get-identity context))]

    (if-not (some #{user} [player1 player2])
      (throw-forbidden {:silent? true}))))

(defon-request add-datomic-db [request]
  (assoc request
    :datomic-db (d/get-db)
    :datomic-conn (d/get-conn)))

(defroutes routes
  [[["/" {:get home-page} ^:interceptors [(body-params/body-params) bootstrap/html-body add-datomic-db]
     ["/users" {:post creat-user}
      ["/:user" {:get user-page} ^:interceptors [(http-basic "Stratego Login" (partial d/credentials (d/get-db)))
                                                 (guard :silent? false)]]]
     ["/game" {:get get-games
               :post [:create-game create-game ^:interceptors [(http-basic "Stratego Login" (partial d/credentials (d/get-db)))
                                                               (guard :silent? false)]]}
      ["/:id" {:get get-game
               #_:put #_[:add-start-pos add-start-pos]}
       ["/moves" {:get get-moves}]
       ["/:index" {:get get-index
                   :put [:make-move make-move ^:interceptors [(http-basic "Stratego Login" (partial d/credentials (d/get-db)))
                                                              (guard :silent? false)
                                                              :unauthorized-fn is-part-of-game?]]}]]]]]])


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

