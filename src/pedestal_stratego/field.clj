(ns pedestal-stratego.field
  (:require [schema.core :as s]
            [clojure.pprint :as p]))

(defmacro dbg [x] `(let [x# ~x] (do (println '~x "=") (p/pprint x#)) x#))

(def Ground (s/enum :water :gras))

(def Rank (s/enum :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :spy :bomb :flag :r0))

(def Move
  "Schema for Move"
  {:from s/Num
   :to s/Num
   (s/optional-key :user) (s/either s/Str s/Num)})

(def Piece
  "Scheme for Piece"
  {:player s/Any
   :rank Rank
   (s/optional-key :possible-move) s/Any})

(def Tile
  "Scheme for Tile"
  {:ground Ground
   :piece (s/maybe Piece)
   (s/optional-key :i) s/Num})

(def Field
  "Schema for Field"
  {s/Num Tile})

(def Direction (s/enum :north :south :east :west))

(def rank-count {:r1 1
                 :r2 1
                 :r3 2
                 :r4 3
                 :r5 4
                 :r6 4
                 :r7 4
                 :r8 5
                 :r9 8
                 :spy 1
                 :bomb 6
                 :flag 1})

(def rank-defeat {:r1 [:bomb]
                  :r2 [:bomb :r1]
                  :r3 [:bomb :r1 :r2]
                  :r4 [:bomb :r1 :r2 :r3]
                  :r5 [:bomb :r1 :r2 :r3 :r4]
                  :r6 [:bomb :r1 :r2 :r3 :r4 :r5]
                  :r7 [:bomb :r1 :r2 :r3 :r4 :r5 :r6]
                  :r8 [:r1 :r2 :r3 :r4 :r5 :r6 :r7]
                  :r9 [:bomb :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8]
                  :spy [:bomb :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9]})


(def all-index (range 1 (inc 100)))

(def water-ver [3 4 7 8])
(def water-hor [4 5])

(defn pair-to-index [x y]
  (+ x (* 10 y)))

(def water
  (for [x water-ver
        y water-hor]
    (pair-to-index x y)))


(def all-index-exept-water
  (vec (sort (clojure.set/difference
              (set all-index)
              (set water)))))


(def empty-field (apply merge (mapv (fn [i] {i {:ground (if (some #{i} water) :water :gras)
                                                :piece nil
                                                :i i}}) all-index)))

(s/defn ^:always-validate get-sorted [board :- Field]
  (mapv second (sort-by first  board)))

(s/defn ^:always-validate field-partition [field]
  (partition 10 field))

(def Fight-Action {:type s/Keyword
                   :tile s/Num
                   :move Move
                   })
(def Remove-Action {:type s/Keyword
                    :tile s/Num
                    })
(def Move-Action {:type s/Keyword
                  :move Move
                  })

(def Action
  (s/either  Fight-Action
             Remove-Action
             Move-Action))

(def Move-Error {:error s/Keyword
                 (s/optional-key :possible) s/Any})

(def Move-Action {:action [Action]
                  :player s/Any
                  (s/optional-key :error) [Move-Error] })

(s/defn ^:always-validate format-piece-console [p :- Piece]
  (condp = (:rank p)
    :r1 "1"
    :r2 "2"
    :r3 "3"
    :r4 "4"
    :r5 "5"
    :r6 "6"
    :r7 "7"
    :r8 "8"
    :r9 "9"
    :spy "S"
    :bomb "B"
    :flag "F"
    :r0 "X"))

(defn format-console [field]
  (mapv (fn [f]
          (if (:piece f)
            (format-piece-console (:piece f))
            (if (= :gras (:ground f)) "_" "O"))) field))

(s/defn ^:always-validate get-piece :- (s/maybe Piece) [f :- Field i :- s/Num]
  (get-in f [i :piece]))

(s/defn ^:always-validate print-console [field :- Field]
  (println (apply str (interpose "\n" (map #(apply str %)
                                            (field-partition
                                             (format-console
                                              (get-sorted field))))))))

(s/defn ^:always-validate can-walk-on :- s/Bool [tile :- Tile, p :- Piece]
  (and (= (:ground tile)
          :gras)
       (not= (:player (:piece tile))
             (:player p))))

(defn valid-index [i]
  (some #{i} all-index-exept-water))


(s/defn not-blocked-by-frindly-piece [f :- Field player :- s/Any i :- s/Num]
  (not= (:player (get-piece f i))
     player))

(s/defn ^:always-validate simple-dir :- (s/maybe s/Num) [i :- s/Num dir :- Direction]
  (when i
    (valid-index (condp = dir
                   :north (+ i 10)
                   :south (- i 10)
                   :west (if (= (mod i 10) 1) nil (dec i))
                   :east (if (= (mod i 10) 0) nil (inc i))))))


(s/defn is-occupied-by-enemy-piece :- s/Bool [f :- Field player :- s/Any i :- s/Num]
  (let [enemy-p (:player (get-piece f i))]
    #_(println "enemy-p: " enemy-p " player: " player "res: " (and enemy-p
          (not= enemy-p
                player)))
     (and enemy-p
          (not= enemy-p
                player))))

(def opposite-direction-lookup
  {:north :south
   :south :north
   :west :east
   :east :west})

(s/defn ^:always-validate valid-move [f :- Field player :- s/Any dir :- Direction i :- s/Num]
  ;;if one move into the other direction, a enemy piece exists, this means the r9 hass to fail on this field,
  ;; for other pieces it does not matter because they will always find themselfs

  (let [o-dir (dir opposite-direction-lookup)
        o-dir-i (simple-dir i o-dir)]

    (when (and (not-blocked-by-frindly-piece f player i)
               (not (is-occupied-by-enemy-piece f player o-dir-i)))

      i)))

(s/defn ^:always-validate surounding-tiles-dir [f :- Field player :- s/Any i :- s/Num dir :- Direction]
  (when (simple-dir i dir)
    (valid-move
     f
     player
     dir
     (simple-dir i dir))))


(s/defn ^:always-validate
  surounding-tiles :- #{s/Num}
  "Return surounding tiles, the assumition is that they are empty"
  [f :- Field player :- s/Any i :- s/Num]
  (set (filter (comp not nil?)
               (mapv valid-index
                    (mapv #(surounding-tiles-dir f player i %)
                         [:north :south :east :west])))))

(s/defn ^:always-validate
  search-direction :- [s/Num]
  [f :- Field player :- s/Any start :- s/Num, dir :- Direction]
  (loop [i start acc []]
    (let [pos (surounding-tiles-dir f player i dir)]
      (if pos
        (recur pos (conj acc pos))
        acc))))

(s/defn ^:always-validate
  surounding-tiles-r9 :- #{[s/Num]}
  "Return surounding tiles for scout (:r9), the assumition is that they are empty"
  [f :- Field player :- s/Any  i :- s/Num]
  (into #{} (map #(search-direction f player i %)
                 [:north :south :east :west])))

(s/defn ^:always-validate
  reachable
  "All fields that can be reached, assuming fields are all empty"
  [f :- Field player :- s/Any i :- s/Num r :- Rank]
  (cond
   (some #{r} [:bomb :flag]) #{}
   (= r :r9) (surounding-tiles-r9 f player i)
   :default (surounding-tiles f player i)))

(s/defn ^:always-validate tile-empty? [f :- Field i :- s/Num]
  (nil? (:piece (get f i))))

(s/defn ^:always-validate tile-occupied-friendly? [f :- Field player :- s/Any i :- s/Num]
  (= (:player (get-piece f i))
     player))

(s/defn ^:always-validate filter-out-friendly [f :- Field player :- s/Any reachble-tiles]
    (vec (filter #(not (tile-occupied-friendly? f player %))
                 reachble-tiles)))

(s/defn ^:always-validate possible-moves [f :- Field i :- s/Num]
  (when-not (tile-empty? f i)
    (let [p (get-piece f i)
          r (:rank p)
          player (:player p)
          possible (reachable f player i r)]

      (if (coll? (first possible))
        (set (apply clojure.set/union possible))
        possible)
      )))


(def sf (assoc-in empty-field [52 :piece]
           {:player 5
            :rank :r9}))

(def sf1 (assoc-in sf [82 :piece]
                   {:player 5
                    :rank :r6}))


(def sf2 (assoc-in sf1 [22 :piece]
                   {:player 4
                    :rank :r7}))

(s/defn random-grouping :- [Rank] []
  (shuffle (mapcat
           (fn [[rank quantity]]
             (repeat quantity rank))
           rank-count)))

(s/defn ^:always-validate possible-moves-field :- s/Any [f :- Field]
  (reduce (fn [field index]
            (if (get-piece field index)
              (update-in field [index :piece] assoc :possible-move (possible-moves field index))
              field))
          f
          all-index-exept-water))


#_(possible-moves-field full-field)

(s/defn ^:always-validate set-piece :- Field [f :- Field i :- s/Num p :- (s/maybe Piece)]
  (assoc-in f [i :piece] p))

(s/defn ^:always-validate set-grouping [field :- Field
                                        grouping :- [Rank]
                                        side :- (s/enum :top :bottum)
                                        player :- s/Any]
  (possible-moves-field (reduce (fn [field [rank pos]]
                                 (set-piece field pos {:rank rank :player player})) field (map #(vector %1 %2) grouping  (if (= :top side)
                                                                                                                           (vec (range 1 41))
                                                                                                                           (mapv #(- 101 %) (range 1 41)))))))

(def r (random-grouping))

(def full-field (set-grouping (set-grouping empty-field (random-grouping) :top 1) (random-grouping) :bottum 2))


#_(possible-moves full-field 40)

#_(print-console full-field)


(s/defn  ^:always-validate move-possible? :- [Move-Error] [f :- Field player :- s/Any move :- Move]
  (let [from (:from move)
        errors [(when-not (= player (:player (get-piece f from)))
                  {:error :from-user})
                (let [moves (possible-moves f from)]
                  (when-not (some #{(:to move)} moves)
                    {:error :not-reachable
                     :possible moves}))]]
    (if (every? nil? errors)
      []
      (vec (filter (comp not nil?) errors)))))


(s/defn ^:always-validate abstract-move :- Move-Action [f :- Field
                                                        player :- s/Any
                                                        move :- Move]
  (let [move-error (move-possible? f player move)]
    (if (empty? move-error)
      (let [to (:to move)
            from (:from move)
            to-player (:player (get-piece f to))]
         {:player player
               :action
               (if to-player
                 [{:type :fight
                   :tile to
                   :move move}
                  {:type :remove
                   :tile from}]
                 [{:type :move
                   :move move}
                  {:type :remove
                   :tile from}])})
      {:player player
       :action []
       :error move-error})))


(s/defn winner :- Piece [from-piece :- Piece
                         to-piece :- Piece]
  (if (some #{(:rank to-piece)} ((:rank from-piece) rank-defeat))
    to-piece
    from-piece))

(s/defn ^:always-validate fight :- Field [f :- Field
                                          i :- s/Num
                                          from-piece :- Piece
                                          to-piece :- Piece]
  (set-piece f
             i
             (cond
              (= (:rank from-piece)
                 (:rank to-piece)) nil
              :else (winner from-piece to-piece))))


(s/defn ^:always-validate execute-move :- Field [f :- Field
                                                 m :- Move]
  (possible-moves-field
   (let [piece1 (get-piece f (:from m))
         piece2 (get-piece f (:to m))]
     (if (nil? piece2)
       (-> f
           (set-piece (:from m) nil)
           (set-piece (:to m) piece1))
       (-> f
           (set-piece (:from m) nil)
           (fight (:to m) piece1 piece2))))))

(s/defn ^:always-validate mask-rank [field :- (s/maybe Field) user :- (s/either s/Str s/Num)]
  (if (nil? field)
    nil
    (apply merge
           (map
            (fn [[k tile]]
              (let [p (:piece tile)
                    piece-owner (:player p)]
                {k
                 (if p
                   (if (= user piece-owner)
                     tile
                     (assoc-in (assoc-in tile [:piece :rank] :r0) [:piece :possible-move] #{}))
                   tile)}))
            field))))
