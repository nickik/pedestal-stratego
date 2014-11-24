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
  (println "2")
  (println obj)
  (if (number? obj)
    (dbg obj)
    (let [e (re-find #"\A-?\d+" obj)]
      (println "-->")
      (if e
        (do (println "-> " e) (Long/parseLong e))
        (dbg obj)))))

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
  (println "get-user-by-name-or-entity")
  (let [user (dbg (maybe-to-long param))
        e (dbg (condp = (type user)
                 String (user-name-to-entity db user)
                 Long user))
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
       :where [?g :game/field ?f]] db))

#_(def g-tx (get-grouping-tx (first (vals (:tempids (creat-game (get-conn) (get-db) "Nick"))))
                           (pedestal-stratego.field/random-grouping)
                           :top
                           [:user/name "Nick"]))


#_(d/transact
     (get-conn)
     g-tx)

#_(add-player-tx 17592186045435 [:user/name "Bio"])

#_(d/transact
  (get-conn)
  (add-player-tx 17592186045435 [:user/name "Bio"]))


#_(q '[:find ?g ?n ?pos ?rank
     :where [?g :game/field ?p]
            [?p :piece/pos ?pos]
            [?p :piece/rank ?ra]
            [?p :piece/owner ?o]
            [?o :user/name ?n]
            [?ra :db/ident ?rank]] (get-db))

#_(q '[:find ?g ?u1 ?u2
     :where [?g :game/player1 ?p1]
            [?g :game/player1 ?p2]
            [?p1 :user/name ?u1]
            [?p2 :user/name ?u2]
            ] (get-db))


#_(creat-piece (get-conn) :piece.rank/r5 (:db/id (get-user-by-name-or-entity (get-db) "Nick")) 1)


#_(q '[:find ?p ?n
     :in $
     :where [?p :game/player1 ?u]
            [?u :user/name ?n]] piece-db)


(defn creat-user [conn user password]
  @(d/transact
    conn
    [{:db/id (d/tempid :db.part/user)
      :user/name user
      :user/password password}]))


#_(
   :piece.rank/r1
   :piece.rank/r2
   :piece.rank/r3
   :piece.rank/r4
   :piece.rank/r5
   :piece.rank/r6
   :piece.rank/r7
   :piece.rank/r8
   :piece.rank/r9
   :piece.rank/spy
   :piece.rank/bomb
   :piece.rank/flag

   :db/ident :user/name
:db/ident :user/password

:db/ident :game/field

:db/ident :piece/rank
:db/ident :piece/owner
:db/ident :piece/pos)
