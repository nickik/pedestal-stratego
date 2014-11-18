(ns pedestal-stratego.game
  (:require [schema.core :as s]
            [clojure.pprint :as p]
            [pedestal-stratego.field :as f]))

(def counter (atom 0))

(defn get-and-inc [counter]
  (let [now @counter]
    (swap! counter inc)))


(def games (atom {}))

(s/defn creat-game [games counter player1 player2 ]
  (let [id (get-and-inc counter)]
    (swap! games assoc id {:player1 player1
                           :player2 player2
                           :id id
                           :field (f/set-grouping (f/set-grouping f/empty-field (f/random-grouping)
                                                                  :top 1)
                                                  (f/random-grouping) :bottum 2)})
    id))
