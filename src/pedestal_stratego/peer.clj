(ns pedestal-stratego.peer
  (:require [schema.core :as s]
            [clojure.pprint :as p]
            [datomic.api :as d :refer (q)]
            [hiccup.core :refer :all]
            [pedestal-stratego.field :as f]))

(defmacro dbg [x] `(let [x# ~x] (do (println '~x "=") (p/pprint x#)) x#))

(def uri "datomic:mem://stratego")

(def schema-tx (read-string (slurp "resources/pedestal_stratego/schema.edn")))
(def data-tx   (read-string (slurp "resources/pedestal_stratego/seed-data.edn")))

(defn init-db []
  (when (d/create-database uri)
    (let [conn (d/connect uri)]
      @(d/transact conn schema-tx)
      @(d/transact conn data-tx))))

(defn del []
  (d/delete-database uri))

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

(s/defn get-rank-keyword [db rank-id]
  (:db/ident (d/pull db '[:db/ident] rank-id)))

(s/defn get-rank [db pieces-map]
  (get-rank-keyword db (:db/id (:piece/rank pieces-map))))

(defn parse-field [db datomic-field]
  (reduce (fn [f piece]
            (f/set-piece f
                         (:piece/pos piece)
                         {:rank  (get-rank-keyword db (:db/id (:piece/rank piece)))
                          :player (:piece/owner piece)}))
          f/empty-field
          datomic-field))

(defn get-full-game [db game-id]
  (d/pull db '[:db/id
               :game/player1
               :game/player2
               {:game/field [*]}] game-id))

(defn maybe-to-long [obj]
  (cond
   (number? obj) obj
   (nil? obj) nil
   (re-find #"\A-?\d+" obj) (Long/parseLong obj)
   :else obj))

(defn user-name-to-entity [db user-name]
  (ffirst
   (q '[:find ?e
        :in $ ?u
        :where [?e :user/name ?u]] db user-name)))

(defn get-user-by-entity-id [db url-for id]
  (let [user (d/pull db '[*] id)]
    (assoc
      user
      :uri
      (url-for
       :pedestal-stratego.service/user-page
       :params {:user (:user/name user)}))))

(defn get-user-by-name-or-entity [db url-for param]
  (let [user (maybe-to-long param)
        e (condp = (type user)
            String  (user-name-to-entity db user)
            Long user
            nil nil)
        full-user (get-user-by-entity-id db url-for e)]
    (when (:db/id full-user)
      full-user)))

(defn get-users [db url-for]
  (map
   #(get-user-by-entity-id db url-for (first %))
    (q '[:find ?e
         :where [?e :user/name ?u]] db)))

#_(get-user-by-name-or-entity (get-db) "Tony")

(defn credentials [db user password]
  (when (= (get-pw db user)
         password)
    {:user user}))

(defn game-tx [id]
  {:db/id (d/tempid :db.part/user)
   :game/player1 id})

(defn add-player-tx [game-id player2-id]
  [:db/add game-id :game/player2 player2-id])

(defn get-piece-tx [game-id rank owner pos]
  (let [tempid (d/tempid :db.part/user)]
    [{:db/id tempid
      :piece/owner owner
      :piece/rank rank
      :piece/pos pos}
     [:db/add game-id :game/field tempid]]))

(s/defn get-grouping-tx [game-id rank-grouping side :- (s/enum :top :bottum) player]
  (mapcat
   #(get-piece-tx game-id %1 player %2)
   rank-grouping
   (if (= :top side)
     (vec (range 1 41))
     (mapv #(- 101 %) (range 1 41)))))

(defn get-games [db]
  (q '[:find ?g
       :where [?g :game/player1 ?f]] db))


(defn get-piece-owner [db game-id index]
  (first (q '[:find ?o ?n
              :in $ ?game-id ?index
              :where
              [?game-id :game/field ?p]
              [?p :piece/owner ?o]
              [?p :piece/pos ?index]
              [?o :user/name ?n]] (get-db) game-id index)))
