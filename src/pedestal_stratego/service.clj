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
            [pedestal-stratego.view :as v]
            [geheimtur.util.auth :as auth :refer [throw-forbidden get-identity]]
            [geheimtur.interceptor :refer [ interactive guard http-basic]]
            [hiccup.core :refer :all]))

(defmacro dbg [x] `(let [x# ~x] (do (println '~x "=") (clojure.pprint/pprint x#)) x#))

(declare routes url-for)

(defn url-for []
  (route/url-for-routes routes))

(defn about-page
  [request]
  (ring-resp/response (format "Clojure %s - served from %s"
                              (clojure-version)
                              (route/url-for ::about-page))))

(defn user-page [request]
  (let [param-user (get-in request [:path-params :user])

        full-param-user (d/get-user-by-name-or-entity (:datomic-db request) param-user)

        loggedin-user (:user (get-identity request))]

    (if full-param-user
      (ring-resp/response (v/view-user full-param-user loggedin-user))
      (ring-resp/not-found (html
                            [:p "User does not exists"])))))

(defn users-page [request]
  (ring-resp/response
   (v/view-users (:uri request)
                 (:datomic-db request)
                 (url-for)
                 (:user (get-identity request)))))

(defn creat-user [request]
  (let [user (:name (:edn-params request))
        pw   (:password (:edn-params request))]
    (ring-resp/response
     (str
      (d/creat-user (:datomic-conn request) user pw)))))

(defn get-games [request]
  (ring-resp/response
   (html (v/view-games (:datomic-db request)
                       (url-for)
                       (:uri request)))))

(defn create-game [request]
  (println "1")
  (let [conn (:datomic-conn request)
        id (g/creat-persistent-game! conn (:user (get-identity request)))]
    (println "2->2")
    (println "type: " (type id))
    (ring-resp/redirect
     (dbg ((url-for) :get-game-route :params {:id id})))))

#_(defn add-start-pos [request]
  (let [id (Integer/parseInt (get-in request [:path-params :id]))
        start (:edn-params request)
        user (:user (get-identity request))]
    (g/add-start (:datomic-conn request)
                 (:datomic-db request)
                 id
                 start
                 user)
    (ring-resp/redirect
     ((url-for) :get-game-route :params {:id id}))))


(defn get-game-route [request]
  (let [id (Long/parseLong (get-in request [:path-params :id]))
        user (:user (get-identity request))
        db (:datomic-db request)]

    (ring-resp/response
     (html (v/view-game db (url-for) (:uri request) id)))))

#_(defn get-index [request]
  (let [id (Integer/parseInt (get-in request [:path-params :id]))
        index (Integer/parseInt (get-in request [:path-params :index]))
        db (:datomic-db request)
        user (:user (get-identity request))]

    (ring-resp/response
     (get (f/mask-rank (:field (g/get-game g/games id)) user) index))))

#_(defn make-move [request]
  (let [id (Integer/parseInt (get-in request [:path-params :id]))
        index (Integer/parseInt (get-in request [:path-params :index]))

        from-piece-moves (:possible-move (f/get-piece (:field (g/get-game g/games id)) index))

        user (:user (get-identity request))

        move {:from index
              :to (some #{(:to (:edn-params request))} from-piece-moves)
              :user user}

        from-rank (get-in (g/get-game g/games id) [:field index :piece :rank])

        to-rank (get-in (g/get-game g/games id) [:field (:to move) :piece :rank])

        player-that-moved-last (:user (g/get-last-move g/games id))]

    (if (= player-that-moved-last
           user)
      (ring-resp/response {:error :not-your-move})
      (do
        (when (:to move)
          (do
            (g/execute-move g/games id move)
            (g/add-move g/games id move)))

        (if (= :flag to-rank)
          (do
            (swap! g/games assoc-in [id :winner] user)
            (ring-resp/response {:winner user}))
          (ring-resp/response {:game (g/get-masked-game g/games id user)
                               :move {:from from-rank
                                      :to to-rank}}))))))

#_(defn get-moves [request]
  (let [id (Integer/parseInt (get-in request [:path-params :id]))]
    (ring-resp/response
     (:moves (g/get-game g/games id)))))

#_(defn is-part-of-game? [context]
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

(defn home-page [request]
   (ring-resp/response
    (html [:body
           [:p [:a {:rel :self  :href (:uri request)} "self"]]
           [:p [:a {:rel :game  :href ((url-for) ::get-games)} "game"]]

           [:p [:a {:rel :users :href ((url-for) ::users-page)} "users"]]])))

(defroutes routes
  [[["/" {:get home-page} ^:interceptors [(body-params/body-params)
                                          bootstrap/html-body
                                          add-datomic-db
                                          (http-basic "Stratego Login" (partial d/credentials (d/get-db)))]

     ["/users" {:get [:user-page-route users-page ^:interceptors []]
                :post creat-user}
      ["/:user" {:get user-page}]]
     ["/games" {:get get-games
                :post [:create-game create-game ^:interceptors [(guard :silent? false)]]}
      ["/:id" {:get [:get-game-route get-game-route ^:interceptors [(guard :silent? false)]]

               #_:put #_[:add-start-pos add-start-pos ^:interceptors [(guard :silent? false)]]}
       #_["/:index" {:get [:get-index get-index ^:interceptors [(guard :silent? false)]]
                   :put [:make-move make-move ^:interceptors [(guard :silent? false)
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
