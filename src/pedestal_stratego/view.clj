(ns pedestal-stratego.view
  (:require [schema.core :as s]
            [clojure.pprint :as p]
            [hiccup.core :refer :all]
            [pedestal-stratego.peer :as d]
            [pedestal-stratego.field :as f]))

(defmacro dbg [x] `(let [x# ~x] (do (println '~x "=") (p/pprint x#)) x#))

(defn wrap [user htm]
  [:html5
   [:head
    [:title "Stratego"]]
   [:body
    [:div "Loged in user: " user]
    [:div
     htm]]])

(defn view-user [full-param-user auth-user]
  (html
   (let [param-user-name (:user/name full-param-user)
         uri (:uri full-param-user)]
     (if full-param-user
       [:div {:itemscope :itemscope :itemtype "http://schema.org/Person"}
        [:p
         [:a {:rel :self :href uri :itemprop :url} param-user-name]]
        [:ul
         (when (= param-user-name auth-user)
           [:li [:p "Password: " [:span {:itemprop :name} (:user/password full-param-user)]]])]]
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

(defn player? [db game-id player-id]
  #_(= player-id)  (do #_(first (datomic.api/q '[:find ?p1 ?p2
                                                       :in $ ?game-id
                                                       :where [?game-id :game/player1 ?p1]
                                                       [?game-id :game/player2 ?p2]
                                                       [?game-id :game/player2 ?p2]] db game-id))
                     :player1))

(defn html-piece [db url-for game-id tile user p1 p2 show-move?]
  (let [piece (:piece tile)
        r (:rank piece)
        player (:db/id (:player piece))
        moves (:possible-move piece)
        uri (url-for :get-index :params {:id game-id :index (:i tile)})]
    [:td {:border 0  :style (str "text-align:center;" (cond
                                                (= (:ground tile) :water) "background-color:blue"
                                                (= (:ground tile) :gras) "background-color:green")) }
     [:a {:rel :self :href uri :style "text-decoration:none"}
      [:h1 {:style (str "font-size:50;"
                        (condp = player
                          (:db/id p1) "color:red"
                          (:db/id p2) "color:blue"
                          player "color:black"))}
       (f/format-piece-console piece)]]
     [:span {:style "font-size:10;"} (:i tile)]
     (when (and (not (empty? moves)) show-move?)
       [:form {:action uri :method :post}
        [:input {:hidden :hidden :value "PUT" :name :method}]
        [:select {:name :to}
         (map
          #(do [:option %])
          moves)]
        [:button {:type :submit} "Move"]])]))


(s/defn ^:always-validate mask-rank [db field :- (s/maybe f/Field) user :- (s/either s/Str s/Num)]
  (if (nil? field)
    nil
    (apply merge
           (map
            (fn [[k tile]]
              (let [p (:piece tile)
                    piece-owner  (:db/id (:player p))
                    user  (:db/id (d/get-user-by-name-or-entity user))]
                {k
                 (if p
                   (if (= user piece-owner)
                     tile
                     (assoc-in (assoc-in tile [:piece :rank] :piece.rank/r0) [:piece :possible-move] #{}))
                   tile)}))
            field))))


(s/defn ^:always-validate field-html [db field :- f/Field user game-id url-for p1 p2]
  (html [:table {:border 0 :style "height:800;width:800px;"}
         (map
          (fn [row]
            [:tr
             (map
              (fn [tile]
                (html-piece db url-for game-id tile user p1 p2 false))
              row)])
          (f/field-partition
           (f/get-sorted (f/mask-rank db field user))))]))

(defn view-game [db url-for user uri game-id]
  (let [game  (d/get-full-game db game-id)
        field   (:game/field game)
        field-asci (f/possible-moves-field (d/parse-field db field))
        p1 (d/get-user-by-name-or-entity db url-for (:db/id (:game/player1 game)))
        p2 (d/get-user-by-name-or-entity db url-for (:db/id (:game/player2 game)))]
    (wrap
     user
     [:div {:itemscope :itemscope :itemtype "http://schema.org/Thing"}
      [:h4 {:itemprop :name} "Game"]
      [:p [:a {:rel :self :href uri :itemprop :url} uri]]
      [:ul
       [:li (view-user p1 nil)]
       [:li (view-user p2 nil)]]
      (when field-asci
        (f/print-console field-asci)
        (field-html db field-asci user game-id url-for p1 p2))])))

(defn view-games [db url-for uri]
  (html
   [:div {:itemscope :itemscope :itemtype "http://schema.org/Thing"}
    [:h3 {:itemprop :name} "Games"]
    [:a {:rel :self :href uri :itemprop :url} uri]

    [:ul {:border 1}
     (map
      (fn [game-id]
        (let [url (url-for :get-game-route :params {:id (first game-id)})]
          (html [:li [:a {:rel (first game-id) :href url} url]])))
      (d/get-games db))]

    #_[:table
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
       (d/get-games db))]

    [:form {:action uri :method :post}
     [:p "Serialised Ranks, Vector 40 len: "]
     [:input {:type :text :name :ranks}]
     [:input {:type :text :name "Content-Type" :disabled :disabled  :hidden :hidden :value "application/edn"}]
     [:input {:type :submit :value :submit}]
     [:p "Example: " (str (f/random-grouping))]]]))
