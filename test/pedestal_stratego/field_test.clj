(ns pedestal-stratego.service-test
  (:require [clojure.test :refer :all]
            [pedestal-stratego.field :as f]))


(deftest surounding-tiles-test
  (are  [x y] (= x y)
        (f/surounding-tiles 5)    #{15 6 4}
        (f/surounding-tiles 1)      #{11 2}
        (f/surounding-tiles 2)    #{12 3 1}
        (f/surounding-tiles 1)     #{11 2}
        (f/surounding-tiles 100)   #{99 90}))


(run-tests)


#_(

   (def sf (assoc-in empty-field [52 :piece]
           {:player 5
            :rank :r9}))

(def sf1 (assoc-in sf [82 :piece]
                   {:player 5
                    :rank :r6}))


(def sf2 (assoc-in sf1 [22 :piece]
                   {:player 4
                    :rank :r7}))

(possible-moves sf 52) #{62 72 92 32 22 51 12 2 82 42}
(possible-moves sf1 52) #{62 72 32 22 51 12 2 42}
(possible-moves sf2 52) #{62 72 32 22 51 42}



   )


#_(


   {:+ {:editor.clj {"pmeta-r" [(:eval.custom
                          "(clojure.test/run-all-tests
                             (re-pattern
                               (str \"^\" *ns* \".*\")))"
                          {:result-type :statusbar})]}}})
