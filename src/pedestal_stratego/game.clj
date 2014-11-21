(ns pedestal-stratego.game
  (:require [schema.core :as s]
            [clojure.pprint :as p]
            [pedestal-stratego.field :as f]
            [pedestal-stratego.peer :as d]))

(def counter (atom 0))

(defn get-and-inc [counter]
  (let [now @counter]
    (swap! counter inc)))

(def games (atom {}))

(s/defn creat-game [games counter player1 player2]

  (d/creat-game (d/get-conn) player1)

  (let [id (get-and-inc counter)]
    (swap! games assoc id {:player1 player1
                           :player2 player2
                           :id id
                           :field f/empty-field #_(f/set-grouping (f/set-grouping f/empty-field (f/random-grouping)
                                                                                  :top 1)
                                                                  (f/random-grouping) :bottum 2)
                           :moves []})
    (f/print-console (get-in @games [id :field]))
    (f/print-console (f/mask-rank (get-in @games [id :field]) player1))
    id))

(defn get-game
  ([games id]
   (get @games id)))

(s/defn add-start [id start user]
  (let [game (get-game games id)
        pos (cond
             (= user (:player1 game)) :top
             :default :bottum)]
    #_(println "id: " id)
    #_(println "start: " start)
    #_(println "start: " pos)
    #_(println "user: " user)
    (if (and (not (= user (:player1 game)))
             (nil? (:player2 game)))
      (swap! games assoc-in [id :player2] user))

    (swap! games update-in [id :field] f/set-grouping start pos user)
    (f/print-console (get-in @games [id :field]))
    (f/print-console (f/mask-rank (get-in @games [id :field]) user))))

(defn add-move [games id move]
  (swap! games update-in [id :moves] conj move))


(defn get-masked-game [games id user]
  (update-in (get-game games id)
             [:field]
             f/mask-rank
             user))

(s/defn execute-move [games id move]
  (swap! games update-in [id :field] f/execute-move move)
  (f/print-console (get-in @games [id :field])))

(s/defn get-last-move [games id]
  (last (:moves (get-game games id))))


