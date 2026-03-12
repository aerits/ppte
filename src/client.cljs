(ns client
  (:require [clojure.string :as str]
            ["react" :as react]
            [reagent.core :as r]
            [goog.string :as gstring]
            [clojure.core.async :as a]
            [reagent.dom.client :as rdomc]))

(defn make-game [[w h] play?]
  [:iframe {:id "frame1"
            :src (gstring/format "index.html?play=%s&server=0" (str play?))
            :height (gstring/format "%ipx" w)
            :width (gstring/format "%ipx" h)}])

(defn app []
  (into [:div
         [make-game [700 600] true]]))
        ; (for [i (range 27)] [make-game [700 600] false])))

(defonce root (delay (rdomc/create-root (.getElementById js/document "root"))))

(defn main []
  (rdomc/render @root [app]))

(a/go
  (a/timeout 10)
  (main))
