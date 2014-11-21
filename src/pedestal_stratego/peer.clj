(ns pedestal-stratego.peer
  (:require [schema.core :as s]
            [clojure.pprint :as p]
            [datomic.api :as d :refer (q)]
            [hiccup.core :refer :all]))

(defmacro dbg [x] `(let [x# ~x] (do (println '~x "=") (p/pprint x#)) x#))

(def uri "datomic:mem://stratego")

(def schema-tx (read-string (slurp "resources/pedestal_stratego/schema.edn")))
(def data-tx   (read-string (slurp "resources/pedestal_stratego/seed-data.edn")))

(defn init-db []
  (when (d/create-database uri)
    (let [conn (d/connect uri)]
      @(d/transact conn schema-tx)
      @(d/transact conn data-tx))))

(defn get-conn []
  (init-db)
  (d/connect uri))

(defn get-db []
  (d/db (get-conn)))

(defn get-pw [db user]
  (ffirst
   (q '[:find ?d
        :in $ ?u
        :where [?e :user/name ?u]
               [?e :user/password ?d]] db user)))

(defn maybe-to-long [obj]
  (let [e (re-find #"\A-?\d+" obj)]
    (if e
      (Long/parseLong e)
      obj)))

(defn user-name-to-entity [db user-name]
  (ffirst
   (q '[:find ?e
        :in $ ?u
        :where [?e :user/name ?u]] db user-name)))

(defn get-user-by-name-or-entity [db param]
  (let [user (maybe-to-long param)
        e (condp = (type user)
            String (user-name-to-entity db user)
            Long user)
        full-user (get-user-by-entity-id db e)]
    (when (:db/id full-user)
      full-user)))


(defn get-user-by-entity-id [db id]
  (let [user (d/pull db '[*] id)]
    (assoc
      user
      :uri
      ((pedestal-stratego.service/url-for)
       :pedestal-stratego.service/user-page
       :params {:user (:user/name user)}))))

(defn get-users [db]
  (map
   #(get-user-by-entity-id db (first %))
    (q '[:find ?e
         :where [?e :user/name ?u]] db)))


#_(get-user-by-name-or-entity (get-db) "Tony")

(defn credentials [db user password]
  (when (= (get-pw db user)
         password)
    {:user user}))

(defn creat-game [conn player1]
  (let [full-user (get-user-by-name-or-entity (d/db conn) player1)]
  @(d/transact
    conn
    [{:db/id #db/id[:db.part/user]
      :game/player1 (:db/id full-user)}])))


(defn creat-user [conn user password]
  @(d/transact
    conn
    [{:db/id #db/id[:db.part/user]
      :user/name user
      :user/password password}]))



