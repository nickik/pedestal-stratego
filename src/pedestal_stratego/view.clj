(ns pedestal-stratego.view
  (:require [schema.core :as s]
            [clojure.pprint :as p]
            [hiccup.core :refer :all]
            [pedestal-stratego.peer :as d]))

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

(defn view-users [uri db auth-user]
  (html
   [:div {:itemscope :itemscope :itemtype "http://schema.org/Thing"}
    [:ul
     [:li [:span {:itemprop :name} "Users"]]
     [:li [:span {:itemprop :url} [:a {:rel :self :href uri} uri]]]
     [:li
      (map #(view-user % auth-user)
           (d/get-users db))]]]))
