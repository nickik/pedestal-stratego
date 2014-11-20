(ns pedestal-stratego.peer
  (:require [schema.core :as s]
            [clojure.pprint :as p]
            [datomic.api :as d :refer (q)]))


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

(defn get-user [db user]
  (ffirst
   (q '[:find ?d
        :in $ ?u
        :where [?e :user/name ?u]
        [?e :user/password ?d]] db user)))

(defn get-pw [db user]
  (ffirst
   (q '[:find ?d
        :in $ ?u
        :where [?e :user/name ?u]
               [?e :user/password ?d]] db user)))

(defn get-user-by-entity [db user-e]
  (:user/name
   (d/pull db [:user/name] user-e)))

(defn entity? [user]
  (let [e (re-find #"\A-?\d+" user)]
    (when e
      (Long/parseLong e))))

(defn credentials [db user password]
  (when (= (get-pw db user)
         password)
    {:user user}))

(defn creat-user [conn user password]
  @(d/transact
    conn
    [{:db/id #db/id[:db.part/user]
      :user/name user
      :user/password password}]))


