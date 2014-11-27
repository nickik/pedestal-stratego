(ns pedestal-stratego.game
  (:require [schema.core :as s]
            [clojure.pprint :as p]
            [pedestal-stratego.field :as f]
            [pedestal-stratego.peer :as d]
            [datomic.api :as datomic :refer (q)]))

(defmacro dbg [x] `(let [x# ~x] (do (println '~x "=") (p/pprint x#)) x#))

(defn creat-persistent-game! [conn player1]
  @(datomic/transact
    conn
    [(d/game-tx [:user/name player1])]))

(defn add-player-2! [conn game-id player2]
  @(datomic/transact
    conn
    [(d/add-player-tx game-id [:user/name player2])]))

(defn add-grouping! [conn game-id rank-grouping side user]
  @(datomic/transact
    conn
    (d/get-grouping-tx game-id
                       rank-grouping
                       side
                       [:user/name user])))

(defn get-field [db game-id]
  (let [game  (q '[:find ?n ?pos ?rank
                   :in $ ?game
                   :where [?game :game/field ?p]
                          [?p :piece/pos ?pos]
                          [?p :piece/rank ?ra]
                          [?p :piece/owner ?o]
                          [?o :user/name ?n]
                          [?ra :db/ident ?rank]] db game-id)]
    (reduce
     (fn [field [user pos rank]]
       (f/set-piece
        field
        pos
        {:rank rank :player user}))
     f/empty-field
     game)))

(s/defn get-player [db game-id]
  (apply merge
         (map (fn [[k v]] {k (:user/name v)})
              (datomic/pull db [{:game/player1 [:user/name]}
                                {:game/player2 [:user/name]}] game-id))))

(defn add-start! [conn db game-id start-rank user]
  (let [players  (get-player db game-id)
        pos  (cond
              (= user
                 (:game/player1 players)) :top
              :default :bottum)]
    (when (and (not (= user
                       (:game/player1 players)))
               (nil? (:game/player2 players)))
      (add-player-2! conn game-id user))
    (add-grouping! conn game-id start-rank pos user)))

(s/defn move-piece-tx [from-id old-pos :- s/Num new-pos :- s/Num]
  [:db.fn/cas from-id :piece/pos old-pos new-pos])

(s/defn delete-piece-tx [piece-id]
  [:db.fn/retractEntity piece-id])

(s/defn winner [db from-piece to-piece]
  (if (some #{(d/get-rank db from-piece)} ((d/get-rank db to-piece)  f/rank-defeat))
    [[:db.fn/retractEntity (:db/id to-piece)]
     (move-piece-tx (:db/id from-piece)
                     (:piece/pos from-piece)
                     (:piece/pos to-piece))]
    [[:db.fn/retractEntity  (:db/id from-piece)]])) ;; move

(defn fight [db game-id  [from-piece to-piece :as pieces]]
  (cond
   (= (:piece/rank from-piece)
      (:piece/rank to-piece)) (map (comp delete-piece-tx :db/id)  pieces)
   :else (winner db from-piece to-piece)))

(defn get-piece [db game-id pos]
  (let [piece-id (ffirst (q '[:find ?p
                              :in $ ?game-id ?pos
                              :where [?game-id :game/field ?p]
                                     [?p :piece/pos  ?pos]] db game-id pos))]
    (datomic/pull db '[*] piece-id)))

(defn execute-move [conn db game-id m]
  (if (= (:from m) (:to m))
    (throw "Cant move to the same field")
    (let [from-piece (get-piece db game-id (:from m))
          to-piece (get-piece db game-id (:to m))]
      (if (nil? (:db/id to-piece))
        [(move-piece-tx (:db/id from-piece) (:from m) (:to m))]
        (fight db game-id [from-piece to-piece])))))
