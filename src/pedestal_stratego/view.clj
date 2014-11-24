(ns pedestal-stratego.view
  (:require [schema.core :as s]
            [clojure.pprint :as p]
            [hiccup.core :refer :all]
            [pedestal-stratego.peer :as d]
            [pedestal-stratego.field :as f]))

(defmacro dbg [x] `(let [x# ~x] (do (println '~x "=") (p/pprint x#)) x#))

(defn view-user [full-param-user auth-user]
  (html
   (let [param-user-name (:user/name full-param-user)
         uri (:uri full-param-user)]
     (if full-param-user
       [:div {:itemscope :itemscope :itemtype "http://schema.org/Person"}
        [:p
         [:span {:itemprop :name} param-user-name]]
        [:ul
         (when (= param-user-name auth-user)
           [:li [:p "Password: " [:span {:itemprop :name} (:user/password full-param-user)]]])
         [:li "Self Relation: "
          [:a {:rel :self :href uri} [:span {:itemprop :url} uri]]]]]
       nil))))

(defn view-users [uri db url-for auth-user]
  (html
   [:div {:itemscope :itemscope :itemtype "http://schema.org/Thing"}
    [:ul
     [:li {:itemprop :name} "Users"]
     [:li [:a {:rel :self :href uri :itemprop :url} uri]]
     [:li
      (map #(view-user % auth-user)
           (d/get-users db url-for))]]]))

(defn view-game [db url-for uri game-id]
  (let [game  (d/get-full-game db game-id)
        field   (:game/field game)
        field-asci (d/parse-field db field)
        p1  (d/get-user-by-name-or-entity db url-for (:db/id (:game/player1 game)))
        p2 (d/get-user-by-name-or-entity db url-for (:db/id (:game/player1 game)))]
    [:div {:itemscope :itemscope :itemtype "http://schema.org/Thing"}
     [:h4 {:itemprop :name} "Game"]
     [:p [:a {:rel :self :href uri :itemprop :url} uri]]
     [:ul
      [:li (view-user p1 nil)]
      [:li (view-user p2 nil)]]
     (when field-asci
       (f/print-console field-asci)
       [:code (f/field-html field-asci)])]))

(defn view-games [db url-for uri]
  (html
   [:div {:itemscope :itemscope :itemtype "http://schema.org/Thing"}
    [:h3 {:itemprop :name} "Games"]
    [:a {:rel :self :href uri :itemprop :url} uri]
    #_[:table {:border 0}
     (map
      (fn [db game-id]
        (let [game-id (first game-id)]
          [:tr
           [:td
            (view-game db
                       url-for
                       (url-for :get-game-route :params {:id game-id})
                       game-id)]]))
      (repeat db)
      (d/get-games db))]]))
